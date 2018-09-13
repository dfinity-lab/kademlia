-- | Network.Kademlia.Process implements all the things that need
-- to happen in the background to get a working Kademlia instance.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Network.Kademlia.Process where

import           Control.Concurrent
                 (ThreadId, forkIO, killThread, myThreadId)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.STM      (atomically, readTVar, writeTVar)
import           Control.Exception           (catch)
import           Control.Monad               (forM_, forever, void, when)
import           Control.Monad.Extra         (unlessM, whenM)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Map                    as M
import           Data.Maybe                  (isNothing)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           System.Random               (newStdGen)

import           Network.Kademlia.Config     (KademliaConfig (..), usingConfig)
import           Network.Kademlia.Instance
                 (KademliaInstance (..), KademliaState (..), deleteValue,
                 insertNode, insertValue, isNodeBanned, lookupNodeByPeer,
                 lookupValue)
import           Network.Kademlia.Networking
                 (KademliaHandle (..), expect, handleLogError', send,
                 startRecvProcess)
import           Network.Kademlia.ReplyQueue
                 (Reply (..), ReplyQueue, ReplyRegistration (..),
                 ReplyType (..), dispatch, expectedReply,
                 replyQueueDispatchChan, replyQueueRequestChan)
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types
                 (Command (..), Node (..), Peer (..), Serialize (..),
                 Signal (..), sortByDistanceTo)
import           Network.Kademlia.Utils      (threadDelay)

-- | Start the background process for a KademliaInstance
start :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
         KademliaInstance i a -> IO ()
start inst = do
    let rq = handleReplyQueue $ instanceHandle inst
    startRecvProcess . instanceHandle $ inst
    receivingId <- forkIO $ receivingProcess inst
    pingId <- forkIO $ pingProcess inst $ replyQueueRequestChan rq
    spreadId <- forkIO $ spreadValueProcess inst
    void . forkIO $ backgroundProcess inst (replyQueueRequestChan rq) [pingId, spreadId, receivingId]

----------------------------------------------------------------------------
-- Main Processes
----------------------------------------------------------------------------
-- There are three main processes: receivingProcess, backgroundProcess and pingProcess.

-- | The central process all Replys go trough
receivingProcess
    :: (Show i, Serialize i, Ord i)
    => KademliaInstance i a
    -> IO ()
receivingProcess inst@(KademliaInstance _ h _ _ _) = forever . (`catch` handleLogError' h) $ do
    let rq = handleReplyQueue h
    reply <- readChan $ replyQueueDispatchChan $ handleReplyQueue h
    let notResponse = not $ isResponse reply
    whenM ((notResponse ||) <$> expectedReply reply rq) $
        receivingProcessDo inst reply rq
  where
    isResponse :: Reply i a -> Bool
    isResponse (Answer (Signal _ PONG))                 = True
    isResponse (Answer (Signal _ (RETURN_VALUE _ _)))   = True
    isResponse (Answer (Signal _ (RETURN_NODES _ _ _))) = True
    isResponse _                                        = False

receivingProcessDo
    :: (Show i, Serialize i, Ord i)
    => KademliaInstance i a
    -> Reply i a
    -> ReplyQueue i a
    -> IO ()
receivingProcessDo inst@(KademliaInstance _ h _ _ cfg) reply rq = do
    handleLogInfo h $ "Received reply: " ++ show reply

    case reply of
        -- Handle a timed out node
        Timeout registration -> do
            let origin = replyOrigin registration

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst origin) $ do
                -- Mark the node as timed out
                pingAgain <- timeoutNode inst origin
                -- If the node should be repinged
                when pingAgain $ do
                    result <- lookupNodeByPeer inst origin
                    case result of
                        Nothing   -> return ()
                        Just node -> sendPing h node (replyQueueRequestChan rq)
            dispatch reply rq -- remove node from ReplyQueue in the last time

        -- Store values in newly encountered nodes that you are the closest to
        Answer (Signal node _) -> do
            let originId = nodeId node
            let peerId = nodePeer node

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst peerId) $ do
                tree <- retrieve stateTree

                -- This node is not yet known
                when (isNothing $ T.lookup tree originId `usingConfig` cfg) $ do
                    let closestKnown = T.findClosest tree originId 1 `usingConfig` cfg
                    let ownId        = T.extractId tree `usingConfig` cfg
                    let self         = node { nodeId = ownId }
                    let bucket       = self:closestKnown
                    -- Find out closest known node
                    let closestId    = nodeId . head $ sortByDistanceTo bucket originId `usingConfig` cfg

                    -- This node can be assumed to be closest to the new node
                    when (ownId == closestId) $ do
                        storedValues <- M.toList <$> retrieveMaybe stateValues
                        let p = nodePeer node
                        -- Store all stored values in the new node
                        forM_ storedValues (send h p . uncurry STORE)
                dispatch reply rq
        Closed -> dispatch reply rq -- if Closed message

  where
    retrieve f = atomically . readTVar . f . instanceState $ inst
    retrieveMaybe f = maybe (pure mempty) (atomically . readTVar) . f . instanceState $ inst

-- | The actual process running in the background
backgroundProcess :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
    KademliaInstance i a -> Chan (Reply i a) -> [ThreadId] -> IO ()
backgroundProcess inst@(KademliaInstance _ h _ _ _) chan threadIds = do
    reply <- liftIO . readChan $ chan

    handleLogInfo h $ "Register chan: reply " ++ show reply

    case reply of
        Answer sig@(Signal (Node peer _) _) -> do
            unlessM (isNodeBanned inst peer) $ do
                handleAnswer sig `catch` handleLogError' h
                repeatBP

        -- Kill all other processes and stop on Closed
        Closed -> do
            mapM_ killThread threadIds

            eThreads <- atomically . readTVar . instanceExpirationThreads $ inst
            mapM_ killThread $ map snd (M.toList eThreads)

        _ -> handleLogInfo h "-- unknown reply" >> repeatBP
  where
    repeatBP = backgroundProcess inst chan threadIds
    handleAnswer sig@(Signal (Node peer _) _) =
        unlessM (isNodeBanned inst peer) $ do
            let node = signalSource sig
            -- Handle the signal
            handleCommand (signalCommand sig) peer inst
            -- Insert the node into the tree, if it's already known, it will
            -- be refreshed
            insertNode inst node

-- | Ping all known nodes every five minutes to make sure they are still present
pingProcess :: KademliaInstance i a
            -> Chan (Reply i a)
            -> IO ()
pingProcess (KademliaInstance _ h (KademliaState sTree _ _) _ cfg) chan = forever . (`catch` handleLogError' h) $ do
    threadDelay (configPingTime cfg)

    tree <- atomically . readTVar $ sTree
    forM_ (T.toList tree) $ \(fst -> node) -> sendPing h node chan

----------------------------------------------------------------------------
-- Helpers and Auxiliary Processes
----------------------------------------------------------------------------

-- | Handles the differendt Kademlia Commands appropriately
handleCommand :: (Serialize i, Ord i) =>
    Command i a -> Peer -> KademliaInstance i a -> IO ()
-- Simply answer a PING with a PONG
handleCommand PING peer inst = send (instanceHandle inst) peer PONG
-- Return a KBucket with the closest Nodes
handleCommand (FIND_NODE nid) peer inst = returnNodes peer nid inst
-- Insert the value into the values store and start the expiration process
handleCommand (STORE key value) _ inst = do
    insertValue key value inst
    void . forkIO . expirationProcess inst $ key
-- Return the value, if known, or the closest other known Nodes
handleCommand (FIND_VALUE key) peer inst = do
    result <- lookupValue key inst
    case result of
        Just value -> liftIO $ send (instanceHandle inst) peer $ RETURN_VALUE key value
        Nothing    -> returnNodes peer key inst
handleCommand _ _ _ = return ()

-- | Store all values stored in the node in the 'k' closest known nodes every hour
spreadValueProcess
    :: (Serialize i)
    => KademliaInstance i a -> IO ()
spreadValueProcess (KademliaInstance _ h (KademliaState sTree _ sValues) _ cfg) = forever . (`catch` handleLogError' h) . void $ do
    threadDelay (configStoreValueTime cfg)

    case sValues of
        Nothing        -> return ()
        Just valueVars -> do
            values <- atomically . readTVar $ valueVars
            tree <- atomically . readTVar $ sTree

            () <$ mapMWithKey (sendRequests tree) values

    where
          sendRequests tree key val = do
            let closest = T.findClosest tree key (configK cfg) `usingConfig` cfg
            forM_ closest $ \node -> send h (nodePeer node) (STORE key val)

          mapMWithKey :: (k -> v -> IO a) -> M.Map k v -> IO [a]
          mapMWithKey f m = sequence . map snd . M.toList . M.mapWithKey f $ m

-- | Delete a value after a certain amount of time has passed
expirationProcess :: (Ord i) => KademliaInstance i a -> i -> IO ()
expirationProcess inst@(KademliaInstance _ _ _ valueTs cfg) key = do
    -- Map own ThreadId to the key
    myTId <- myThreadId
    oldTId <- atomically $ do
        threadIds <- readTVar valueTs
        writeTVar valueTs $ M.insert key myTId threadIds
        return . M.lookup key $ threadIds

    -- Kill the old timeout thread, if it exists
    mapM_ killThread oldTId

    threadDelay (configExpirationTime cfg)
    deleteValue key inst

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes :: (Serialize i, Ord i) =>
    Peer -> i -> KademliaInstance i a -> IO ()
returnNodes peer nid (KademliaInstance ourNode h (KademliaState sTree _ _) _ cfg@KademliaConfig {..}) = do
    tree           <- atomically . readTVar $ sTree
    rndGen         <- newStdGen
    let closest     = T.findClosest tree nid configK `usingConfig` cfg
    let randomNodes = T.pickupRandom tree configRoutingSharingN closest rndGen
    -- Must never give an empty list. The networking part assumes that there
    -- will always be at least one node. If there is nothing, then it's not
    -- clear what to send to the peer, and so nothing is sent, and the peer
    -- times out. This causes joinNetwork to time out for the first node to
    -- join (the existing node doesn't know any peers).
    let nodes       = case closest ++ randomNodes of
                          [] -> [ourNode]
                          xs -> xs
    liftIO $ send h peer (RETURN_NODES 1 nid nodes)

-- Send PING and expect a PONG
sendPing :: KademliaHandle i a -> Node i -> Chan (Reply i a) -> IO ()
sendPing h node chan = do
    expect h (RR [R_PONG] (nodePeer node)) $ chan
    send h (nodePeer node) PING

-- | Signal a Node's timeout and retur wether it should be repinged
timeoutNode :: (Serialize i, Ord i) => KademliaInstance i a -> Peer -> IO Bool
timeoutNode (KademliaInstance _ _ (KademliaState sTree _ _) _ cfg) peer = do
    currentTime <- floor <$> getPOSIXTime
    atomically $ do
        tree <- readTVar sTree
        let (newTree, pingAgain) = T.handleTimeout currentTime tree peer `usingConfig` cfg
        writeTVar sTree newTree
        pure pingAgain

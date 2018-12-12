--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

--------------------------------------------------------------------------------

-- |
-- "DFINITY.Discovery.Process" implements all the things that need
-- to happen in the background to get a working Kademlia instance.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Process
  ( start
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent
                 (ThreadId, forkIO, killThread, myThreadId)
import           Control.Concurrent.Chan      (Chan, readChan)
import           Control.Concurrent.STM       (atomically, readTVar, writeTVar)
import           Control.Exception            (catch)
import           Control.Monad                (forM_, forever, void, when)
import           Control.Monad.Extra          (unlessM)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (isNothing)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           System.Random                (newStdGen)

import           DFINITY.Discovery.Config     (KademliaConfig (..), usingConfig)
import           DFINITY.Discovery.Instance
                 (KademliaInstance (..), KademliaState (..), deleteValue,
                 insertNode, insertValue, isNodeBanned, lookupNodeByPeer,
                 lookupValue)
import           DFINITY.Discovery.Networking
                 (KademliaHandle (..), expect, handleLogError', send,
                 startRecvProcess)
import           DFINITY.Discovery.ReplyQueue
                 (Reply (..), ReplyQueue, ReplyRegistration (..),
                 ReplyType (..), dispatch, expectedReply,
                 replyQueueDispatchChan, replyQueueRequestChan)
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Command (..), Node (..), Peer (..), Serialize (..),
                 Signal (..), sortByDistanceTo)
import           DFINITY.Discovery.Utils      (threadDelay)

--------------------------------------------------------------------------------

-- | Start the background process for a 'KademliaInstance'.
start
  :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
  => KademliaInstance i a -> IO ()
start inst = do
    let rq = handleReplyQueue $ instanceHandle inst
    startRecvProcess . instanceHandle $ inst
    receivingId <- forkIO $ receivingProcess inst
    pingId <- forkIO $ pingProcess inst $ replyQueueRequestChan rq
    spreadId <- forkIO $ spreadValueProcess inst
    void . forkIO $ backgroundProcess inst (replyQueueRequestChan rq) [pingId, spreadId, receivingId]

--------------------------------------------------------------------------------

-- | The central process all 'Reply's go through.
receivingProcess
  :: (Show i, Serialize i, Ord i)
  => KademliaInstance i a
  -> IO ()
receivingProcess inst = do
  let (KademliaInstance _ h _ _ _) = inst

  let isResponse :: Reply i a -> Bool
      isResponse (Answer (Signal _ PONG))                 = True
      isResponse (Answer (Signal _ (RETURN_NODES _ _ _))) = True
      isResponse _                                        = False

  forever $ (`catch` handleLogError' h) $ do
    let rq = handleReplyQueue h
    reply <- readChan $ replyQueueDispatchChan rq
    let notResponse = not $ isResponse reply
    wasExpected <- expectedReply reply rq
    when (notResponse || wasExpected) $ do
      receivingProcessDo inst reply rq

receivingProcessDo
  :: (Show i, Serialize i, Ord i)
  => KademliaInstance i a
  -> Reply i a
  -> ReplyQueue i a
  -> IO ()
receivingProcessDo inst reply rq = do
  let (KademliaInstance _ h _ _ cfg) = inst

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
      let retrieve f = atomically (readTVar (f (instanceState inst)))

      -- If peer is banned, ignore
      unlessM (isNodeBanned inst (nodePeer node)) $ do
        tree <- retrieve stateTree

        -- This node is not yet known
        when (isNothing (T.lookup tree originId `usingConfig` cfg)) $ do
          let closestKnown = T.findClosest tree originId 1 `usingConfig` cfg
          let ownId        = T.extractId tree `usingConfig` cfg
          let self         = node { nodeId = ownId }
          let bucket       = self:closestKnown
          -- Find out closest known node
          let closestId    = nodeId (head (sortByDistanceTo bucket originId `usingConfig` cfg))


          -- This node can be assumed to be closest to the new node
          when (ownId == closestId) $ do
            storedValues <- Map.toList <$> retrieve stateValues
            -- Store all stored values in the new node
            forM_ storedValues (send h (nodePeer node) . uncurry STORE)
        dispatch reply rq

    -- If a Closed message is received
    Closed -> dispatch reply rq

-- | The actual process running in the background
backgroundProcess
  :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
  => KademliaInstance i a
  -> Chan (Reply i a)
  -> [ThreadId]
  -> IO ()
backgroundProcess inst@(KademliaInstance _ h _ _ _) chan threadIds = do
  reply <- liftIO $ readChan chan

  handleLogInfo h $ "Register chan: reply " ++ show reply

  let repeatBP = backgroundProcess inst chan threadIds

  let handleAnswer sig@(Signal (Node peer _) _) = do
        unlessM (isNodeBanned inst peer) $ do
          let node = signalSource sig
          -- Handle the signal
          handleCommand (signalCommand sig) peer inst
          -- Insert the node into the tree; if it's already known,
          -- it will be refreshed
          insertNode inst node

  case reply of
    Answer sig@(Signal (Node peer _) _) -> do
      unlessM (isNodeBanned inst peer) $ do
        handleAnswer sig `catch` handleLogError' h
        repeatBP
    -- Kill all other processes and stop on Closed
    Closed -> do
      mapM_ killThread threadIds
      eThreads <- atomically $ readTVar $ instanceExpirationThreads inst
      mapM_ (killThread . snd) (Map.toList eThreads)
    _ -> do
      handleLogInfo h "-- unknown reply"
      repeatBP

-- | Ping all known nodes every five minutes to make sure they are still present
pingProcess
  :: KademliaInstance i a
  -> Chan (Reply i a)
  -> IO ()
pingProcess inst chan = do
  let (KademliaInstance _ h state _ cfg) = inst
  let (KademliaState sTree _ _) = state
  forever $ (`catch` handleLogError' h) $ do
    threadDelay (configPingTime cfg)
    tree <- atomically $ readTVar sTree
    forM_ (T.toList tree) $ \(fst -> node) -> sendPing h node chan

-- | Store all values stored in the node in the 'k' closest known nodes every hour
spreadValueProcess
    :: (Serialize i)
    => KademliaInstance i a -> IO ()
spreadValueProcess inst = do
  let (KademliaInstance _ h state _ cfg) = inst
  let (KademliaState sTree _ sValues) = state
  forever $ (`catch` handleLogError' h) $ void $ do
    threadDelay (configStoreValueTime cfg)

    let sendRequests tree key val = do
          let closest = T.findClosest tree key (configK cfg) `usingConfig` cfg
          forM_ closest $ \node -> send h (nodePeer node) (STORE key val)

    let mapMWithKey_ :: (k -> v -> IO a) -> Map k v -> IO ()
        mapMWithKey_ f m = mapM_ snd (Map.toList (Map.mapWithKey f m))

    values <- atomically (readTVar sValues)
    tree   <- atomically (readTVar sTree)
    mapMWithKey_ (sendRequests tree) values

-- | Delete a value after a certain amount of time has passed
expirationProcess :: (Ord i) => KademliaInstance i a -> i -> IO ()
expirationProcess inst key = do
  let (KademliaInstance _ _ _ valueTs cfg) = inst

  -- Map own ThreadId to the key
  myTId <- myThreadId
  oldTId <- atomically $ do
    threadIds <- readTVar valueTs
    writeTVar valueTs $ Map.insert key myTId threadIds
    pure (Map.lookup key threadIds)

  -- Kill the old timeout thread, if it exists
  mapM_ killThread oldTId

  threadDelay (configExpirationTime cfg)
  deleteValue key inst

--------------------------------------------------------------------------------

-- | Handles the different Kademlia Commands appropriately.
handleCommand
  :: (Serialize i, Ord i)
  => Command i a
  -> Peer
  -> KademliaInstance i a
  -> IO ()
handleCommand cmd peer inst
  = case cmd of
      -- Simply answer a 'PING' with a 'PONG'.
      PING                 -> send (instanceHandle inst) peer PONG
      -- Return a 'KBucket' with the closest 'Node's.
      (FIND_NODE nid)      -> returnNodes peer nid inst
      -- In all other cases, do nothing.
      PONG                 -> pure ()
      (RETURN_NODES _ _ _) -> pure ()
      -- Insert the value into the values store and start the expiration process
      (STORE key value)    -> do
        insertValue key value inst
        void $ forkIO $ expirationProcess inst key
      -- Return the value, if known, or the closest other known Nodes
      (FIND_VALUE key)     -> do
        result <- lookupValue key inst
        let h = instanceHandle inst
        case result of
          Just value -> liftIO $ send h peer (RETURN_VALUE key value)
          Nothing    -> returnNodes peer key inst
      (RETURN_VALUE _ _)   -> pure ()

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes
  :: (Serialize i, Ord i)
  => Peer
  -> i
  -> KademliaInstance i a
  -> IO ()
returnNodes peer nid inst = do
  let (KademliaInstance ourNode h state _ cfg) = inst
  let (KademliaState sTree _ _) = state
  let (KademliaConfig {..}) = cfg
  tree <- atomically (readTVar sTree)
  rndGen <- newStdGen
  let closest     = T.findClosest tree nid configK `usingConfig` cfg
  let randomNodes = T.pickupRandom tree configRoutingSharingN closest rndGen
  -- Must never give an empty list. The networking part assumes that there
  -- will always be at least one node. If there is nothing, then it's not
  -- clear what to send to the peer, and so nothing is sent, and the peer
  -- times out. This causes joinNetwork to time out for the first node to
  -- join (the existing node doesn't know any peers).
  let nodes = case closest ++ randomNodes of
                [] -> [ourNode]
                xs -> xs
  liftIO $ send h peer (RETURN_NODES 1 nid nodes)

-- | Send 'PING' and expect a 'PONG'.
sendPing
  :: KademliaHandle i a
  -> Node i
  -> Chan (Reply i a)
  -> IO ()
sendPing h node chan = do
  expect h (ReplyRegistration [R_PONG] (nodePeer node)) $ chan
  send h (nodePeer node) PING

-- | Signal a node timeout and return whether it should be repinged.
timeoutNode
  :: (Serialize i, Ord i)
  => KademliaInstance i a
  -> Peer
  -> IO Bool
timeoutNode (KademliaInstance _ _ (KademliaState sTree _ _) _ cfg) peer = do
  currentTime <- floor <$> getPOSIXTime
  atomically $ do
    tree <- readTVar sTree
    let (newTree, pingAgain) = T.handleTimeout currentTime tree peer
                               `usingConfig` cfg
    writeTVar sTree newTree
    pure pingAgain

--------------------------------------------------------------------------------

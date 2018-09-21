--------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

--------------------------------------------------------------------------------

-- |
-- Network.Kademlia.Process implements all the things that need
-- to happen in the background to get a working Kademlia instance.

--------------------------------------------------------------------------------

module Network.Kademlia.Process
  ( start
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent          (ThreadId, forkIO, killThread)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.STM      (atomically, readTVar, writeTVar)
import           Control.Exception           (catch)
import           Control.Monad               (forM_, forever, when)
import           Control.Monad.Extra         (unlessM)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Map                    as Map
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           System.Random               (newStdGen)

import           Network.Kademlia.Config     (KademliaConfig (..), usingConfig)
import           Network.Kademlia.Instance
                 (KademliaInstance (..), KademliaState (..), insertNode,
                 isNodeBanned, lookupNodeByPeer)
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
                 Signal (..))
import           Network.Kademlia.Utils      (threadDelay)

--------------------------------------------------------------------------------

-- | Start the background process for a 'KademliaInstance'.
start
  :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
  => KademliaInstance i a -> IO ()
start inst = do
  let handle = instanceHandle inst
  startRecvProcess handle
  let chan = replyQueueRequestChan (handleReplyQueue handle)
  receivingId <- forkIO $ receivingProcess inst
  pingId      <- forkIO $ pingProcess inst chan
  _           <- forkIO $ backgroundProcess inst chan [pingId, receivingId]
  pure ()

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
  let (KademliaInstance _ h _ _ _) = inst

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
      -- If peer is banned, ignore
      unlessM (isNodeBanned inst (nodePeer node)) $ do
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
  let (KademliaState sTree _) = state
  forever $ (`catch` handleLogError' h) $ do
    threadDelay (configPingTime cfg)
    tree <- atomically $ readTVar sTree
    forM_ (T.toList tree) $ \(fst -> node) -> sendPing h node chan

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

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes
  :: (Serialize i, Ord i)
  => Peer
  -> i
  -> KademliaInstance i a
  -> IO ()
returnNodes peer nid inst = do
  let (KademliaInstance ourNode h state _ cfg) = inst
  let (KademliaState sTree _) = state
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
timeoutNode (KademliaInstance _ _ (KademliaState sTree _) _ cfg) peer = do
  currentTime <- floor <$> getPOSIXTime
  atomically $ do
    tree <- readTVar sTree
    let (newTree, pingAgain) = T.handleTimeout currentTime tree peer
                               `usingConfig` cfg
    writeTVar sTree newTree
    pure pingAgain

--------------------------------------------------------------------------------

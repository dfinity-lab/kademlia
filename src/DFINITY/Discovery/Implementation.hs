--------------------------------------------------------------------------------

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Implementation
-- Description: The details of the lookup algorithm
--
-- "DFINITY.Discovery.Implementation" contains the actual implementations of the
-- different Kademlia Network Algorithms.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Implementation
  ( joinNetwork
  , JoinResult (..)
  , lookupNode
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow                ((>>>))
import           Control.Concurrent.Chan      (Chan, newChan, readChan)
import           Control.Concurrent.STM       (atomically, readTVar)
import           Control.Monad                (forM_, when)
import           Control.Monad.Extra          (unlessM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Class    (MonadTrans (lift))
import           Control.Monad.Trans.State    (StateT, evalStateT, gets, modify)
import           Data.List                    (delete, find)
import qualified Data.Map                     as M
import           Data.Word                    (Word8)

import           DFINITY.Discovery.Config     (KademliaConfig (..), usingConfig)
import           DFINITY.Discovery.Instance
                 (KademliaInstance (..), KademliaState (..), insertNode,
                 isNodeBanned)
import           DFINITY.Discovery.Networking (expect, send)
import           DFINITY.Discovery.ReplyQueue
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Command (..), Node (..), Peer, Serialize (..), Signal (..),
                 sortByDistanceTo)

--------------------------------------------------------------------------------

-- | The different possible results of joinNetwork
data JoinResult
  = JoinSuccess
  | NodeDown
  | IDClash
  | NodeBanned
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Make a KademliaInstance join the network a supplied Node is in
joinNetwork
  :: (Serialize i, Serialize a, Ord i)
  => KademliaInstance i a
  -> Peer
  -> IO JoinResult
joinNetwork inst initPeer
  = ownId >>= runLookup go inst
  where
    go = do
      -- If node is banned, quit
      banned <- liftIO $ isNodeBanned inst initPeer
      if banned
        then pure NodeBanned
        else (do -- Poll the supplied node
                 -- liftIO $ putStrLn $ "join: sending to " ++ show (peer node)
                 sendSFirst initPeer
                 -- Run a normal lookup from there out
                 waitForReplyDo True (pure NodeDown) checkSignal)

    -- Retrieve your own id
    ownId = do
      tree <- atomically (readTVar (stateTree (instanceState inst)))
      pure (T.extractId tree)

    -- Also insert all returned nodes to our bucket (see [CSL-258])
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) = do
      -- Check whether the own id was encountered. If so, return a IDClash
      -- error, otherwise, continue the lookup.
      -- Commented out due to possibility of bug (like when node reconnects)
      tId <- gets lookupStateTargetId
      case find (\retNode -> nodeId retNode == tId) nodes of
          Just _ -> return IDClash
          _      -> continueLookup nodes sendS continue finish
      -- continueLookup nodes sendS continue finish

    checkSignal _ = error "Unknown signal for @joinNetwork@"

    -- Continuing always means waiting for the next signal
    continue = waitForReply finish checkSignal

    -- Send a FIND_NODE command, looking up your own id
    sendSFirst p = lift ownId >>= (FIND_NODE >>> flip sendSignalWithoutPolled p)
    sendS      p = lift ownId >>= (FIND_NODE >>> flip sendSignal p)

    -- Return a success, when the operation finished cleanly
    finish = return JoinSuccess

--------------------------------------------------------------------------------

-- |
-- Lookup the Node corresponding to the supplied ID
lookupNode
  :: forall i a.
     (Serialize i, Serialize a, Ord i)
  => KademliaInstance i a
  -> i
  -> IO (Maybe (Node i))
lookupNode inst nid = runLookup go inst nid
  where
    go :: LookupM i a (Maybe (Node i))
    go = startLookup (instanceConfig inst) sendS end checkSignal

    -- Return empty list on lookup failure
    end :: LookupM i a (Maybe (Node i))
    end = pure Nothing

    -- Check whether the Node we are looking for was found.
    --
    -- There are two cases after receiving:
    -- 1. If we didn't find the node then continue lookup.
    -- 2. Otherwise return the found node.
    --
    -- Also insert all returned nodes to our tree.
    checkSignal :: Signal i v -> LookupM i a (Maybe (Node i))
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) = do
      let targetNode = find ((== nid) . nodeId) nodes
      case targetNode of
        Nothing  -> continueLookup nodes sendS continue end
        justNode -> pure justNode
    checkSignal _ = end -- TODO: maybe it should be `panic` if we get some other return result

    -- Continuing always means waiting for the next signal
    continue :: LookupM i a (Maybe (Node i))
    continue = waitForReply end checkSignal

    -- Send a 'FIND_NODE' command looking for the node corresponding to
    -- the given 'Node'.
    sendS :: Node i -> LookupM i a ()
    sendS = sendSignal (FIND_NODE nid)

----------------------------------------------------------------------------

-- | The state of a lookup.
data LookupState i a
  = LookupState
    { lookupStateInstance  :: !(KademliaInstance i a)
    , lookupStateTargetId  :: !i
    , lookupStateReplyChan :: !(Chan (Reply i a))
    , lookupStateKnown     :: ![Node i]
    , lookupStatePending   :: !(M.Map (Node i) Word8)
    , lookupStatePolled    :: ![Node i]
    }
  deriving ()

----------------------------------------------------------------------------

-- | Monad transformer for a lookup.
type LookupM i a = StateT (LookupState i a) IO

-- | Run a 'LookupM', returning its result.
runLookup
  :: (Ord i)
  => LookupM i a b
  -> KademliaInstance i a
  -> i
  -> IO b
runLookup lookupM inst nid = do
  chan <- newChan
  let state = LookupState inst nid chan mempty mempty mempty
  evalStateT lookupM state

----------------------------------------------------------------------------

-- |
-- The initial phase of the normal Kademlia lookup operation
startLookup
  :: (Serialize i, Serialize a, Ord i)
  => KademliaConfig
  -> (Node i -> LookupM i a ())
  -> LookupM i a b
  -> (Signal i a -> LookupM i a b)
  -> LookupM i a b
startLookup cfg signalAction cancel onSignal = do
  inst  <- gets lookupStateInstance
  tree  <- liftIO . atomically . readTVar . stateTree . instanceState $ inst
  nid   <- gets lookupStateTargetId

  let closest = T.findClosest tree nid (configNumLookupNodes cfg)
                `usingConfig` cfg

  -- Find the three nodes closest to the supplied id
  if null closest
    then cancel
    else (do -- Add them to the list of known nodes.
             -- At this point, it will be empty, therefore just overwrite it.
             modify $ \s -> s { lookupStateKnown = closest }

             -- Send a signal to each of the Nodes
             forM_ closest signalAction

             -- Start the recursive lookup
             waitForReply cancel onSignal)

----------------------------------------------------------------------------

-- Wait for the next reply and handle it appropriately
waitForReply
  :: (Serialize i, Serialize a, Ord i)
  => LookupM i a b
  -> (Signal i a -> LookupM i a b)
  -> LookupM i a b
waitForReply = waitForReplyDo False

--------------------------------------------------------------------------------

-- Wait for the next reply and handle it appropriately
waitForReplyDo
  :: (Serialize i, Serialize a, Ord i)
  => Bool
  -> LookupM i a b
  -> (Signal i a -> LookupM i a b)
  -> LookupM i a b
waitForReplyDo withinJoin cancel onSignal = do
  chan <- gets lookupStateReplyChan
  inst <- gets lookupStateInstance

  let modifyPending f s = s { lookupStatePending = f (lookupStatePending s) }
  let modifyPolled  f s = s { lookupStatePolled  = f (lookupStatePolled  s) }
  let modifyKnown   f s = s { lookupStateKnown   = f (lookupStateKnown   s) }

  -- Remove the node from the list of nodes with pending replies
  let removeFromPending peer
        = modify (modifyPending (M.delete peer))

  -- Remove every trace of the node's existance
  let removeFromEverywhere node
        = modify (modifyPending  (M.delete node)
                  . modifyKnown  (delete node)
                  . modifyPolled (delete node))

  -- FIXME: doc
  let removeFromEverywherePeer peer
        = modify (modifyPending  (M.filterWithKey (\k _ -> nodePeer k /= peer))
                  . modifyKnown  (filter ((/= peer) . nodePeer))
                  . modifyPolled (filter ((/= peer) . nodePeer)))

  -- Continue, if there still are pending responses
  let continueIfMorePending = do
        updatedPending <- gets lookupStatePending
        if not (null updatedPending)
          then waitForReply cancel onSignal
          else cancel

  result <- liftIO $ readChan chan
  case result of
    -- If there was a reply
    Answer sig@(Signal node cmd) -> do
      banned <- liftIO $ isNodeBanned inst (nodePeer node)

      if banned
        then (do -- Ignore message from banned node, wait for another message
                 removeFromEverywhere node
                 continueIfMorePending)
        else (do when withinJoin $ do
                   -- Mark the node as polled and pending
                   modify (modifyPending (M.insert node 0))
                   modify (modifyPolled  (node:))

                 -- Insert the node into the tree, as it might be a new one or
                 -- it would have to be refreshed
                 liftIO $ insertNode inst node

                 case cmd of
                   RETURN_NODES n nid _ -> do
                     toRemove <- maybe True (\s -> s + 1 >= n)
                                 <$> gets (M.lookup node . lookupStatePending)
                     if toRemove
                       then removeFromPending node
                       else do modify (modifyPending (M.adjust (+1) node))
                               let reg = ReplyRegistration
                                         [R_RETURN_NODES nid]
                                         (nodePeer node)
                               liftIO $ expect (instanceHandle inst) reg chan
                   _ -> removeFromPending node

                 -- Call the signal handler
                 onSignal sig)

    -- On timeout
    Timeout registration -> do
      let peer = replyOrigin registration
      removeFromEverywherePeer peer
      continueIfMorePending

    Closed -> cancel

--------------------------------------------------------------------------------

-- |
-- Decide whether and which node to poll and react appropriately.
--
-- This is the meat of Kademlia lookups.
continueLookup
  :: (Serialize i, Eq i)
  => [Node i]
  -> (Node i -> LookupM i a ())
  -> LookupM i a b
  -> LookupM i a b
  -> LookupM i a b
continueLookup nodes signalAction continue end = do
  let closest
        :: (Serialize i)
        => KademliaInstance i a
        -> [Node i]
        -> LookupM i a [Node i]
      closest inst known = do
        cid    <- gets lookupStateTargetId
        polled <- gets lookupStatePolled
        let cfg = instanceConfig inst

        -- Return the k closest nodes, the lookup had contact with
        pure (take
              (configK cfg)
              (sortByDistanceTo (known ++ polled) cid))

  let allClosestPolled
        :: (Eq i, Serialize i)
        => KademliaInstance i a
        -> [Node i]
        -> LookupM i a Bool
      allClosestPolled inst known = do
        polled       <- gets lookupStatePolled
        closestKnown <- closest inst known
        pure (all (`elem` polled) closestKnown)

  inst    <- gets lookupStateInstance
  known   <- gets lookupStateKnown
  nid     <- gets lookupStateTargetId
  pending <- gets lookupStatePending
  polled  <- gets lookupStatePolled

  let cfg = instanceConfig inst

  -- Pick the k closest known nodes that haven't been polled yet
  let newKnown = take (configK cfg)
                 $ (\xs -> sortByDistanceTo xs nid)
                 $ filter (`notElem` polled) (nodes ++ known)

  -- Check if k closest nodes have been polled already
  polledNeighbours <- allClosestPolled inst newKnown

  if | not (null newKnown) && not polledNeighbours -> do
         -- Send signal to the closest node that hasn't been polled yet
         let next = head (sortByDistanceTo newKnown nid)
         signalAction next

         -- Update known
         modify $ \s -> s { lookupStateKnown = newKnown }

         -- Continue the lookup
         continue
     | not (null pending) -> do
         -- Wait for the pending replies to finish
         continue
     | otherwise -> do
         -- Stop recursive lookup
         end

--------------------------------------------------------------------------------

-- |
-- Send a signal to a node.
sendSignalWithoutPolled
  :: Ord i
  => Command i a
  -> Peer
  -> LookupM i a ()
sendSignalWithoutPolled cmd peer = do
  inst <- gets lookupStateInstance

  -- Not interested in results from banned node
  unlessM (liftIO (isNodeBanned inst peer)) $ do
    chan <- gets lookupStateReplyChan
    let h = instanceHandle inst

    -- Send the signal
    liftIO $ send h peer cmd

    -- Determine the appropriate ReplyRegistrations to the command
    let regs = case cmd of
          FIND_NODE nid -> ReplyRegistration [R_RETURN_NODES nid] peer
          _             -> error "Unknown command at @sendSignal@"

    -- Expect an appropriate reply to the command
    liftIO $ expect h regs chan

--------------------------------------------------------------------------------

-- |
-- Send a signal to a node
sendSignal
  :: Ord i
  => Command i a
  -> Node i
  -> LookupM i a ()
sendSignal cmd node = do
  inst <- gets lookupStateInstance

  unlessM (liftIO (isNodeBanned inst (nodePeer node))) $ do
    sendSignalWithoutPolled cmd (nodePeer node)
    polled  <- gets lookupStatePolled
    pending <- gets lookupStatePending
    -- Mark the node as polled and pending
    modify $ \s -> s { lookupStatePolled  = node : polled
                     , lookupStatePending = M.insert node 0 pending
                     }

--------------------------------------------------------------------------------

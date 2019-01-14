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
  ( lookup
  , store
  , joinNetwork
  , JoinResult (..)
  , lookupNode
  ) where

--------------------------------------------------------------------------------

import           Prelude                      hiding (lookup)

import           Control.Arrow                ((>>>))
import           Control.Concurrent.Chan      (Chan, newChan, readChan)
import           Control.Concurrent.STM       (atomically, readTVar)
import           Control.Monad                (forM_, unless, when)
import           Control.Monad.Extra          (unlessM)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Class    (MonadTrans (lift))
import           Control.Monad.Trans.State    (StateT, evalStateT, gets, modify)
import           Data.List                    (delete, find, (\\))
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Word                    (Word8)

import           DFINITY.Discovery.Config     (KademliaConfig (..), usingConfig)
import           DFINITY.Discovery.Instance
                 (KademliaInstance (..), KademliaState (..), insertNode,
                 isNodeBanned)
import           DFINITY.Discovery.Networking (expect, send)
import           DFINITY.Discovery.ReplyQueue
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Command (..), Ident, Node (..), Peer, Signal (..), Value,
                 sortByDistanceTo)

--------------------------------------------------------------------------------

-- | Lookup the value corresponding to a key in the DHT and return it, together
--   with the Node that was the first to answer the lookup
lookup
  :: KademliaInstance -> Ident -> IO (Maybe (Value, Node))
lookup inst nid = runLookup go inst nid
  where
    go = startLookup (instanceConfig inst) sendS cancel checkSignal

    -- Return Nothing on lookup failure
    cancel = pure Nothing

    -- When receiving a RETURN_VALUE command, finish the lookup, then
    -- cache the value in the closest peer that didn't return it and
    -- finally return the value
    checkSignal (Signal origin (RETURN_VALUE _ value)) = do
      -- Abuse the known list for saving the peers that are *known* to
      -- store the value
      modify $ \s -> s { lookupStateKnown = [origin] }

      -- Finish the lookup, recording which nodes returned the value
      finish

      -- Store the value in the closest peer that didn't return the
      -- value
      known  <- gets lookupStateKnown
      polled <- gets lookupStatePolled
      let rest = polled \\ known
      unless (null rest) $ do
        let cachePeer = nodePeer $ head $ sortByDistanceTo nid rest
        liftIO (send (instanceHandle inst) cachePeer (STORE nid value))

      -- Return the value
      pure (Just (value, origin))

    -- When receiving a RETURN_NODES command, throw the nodes into the
    -- lookup loop and continue the lookup
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) =
      continueLookup nodes sendS continue cancel

    checkSignal _ = error "Fundamental error in unhandled query @lookup@"

    -- Continuing always means waiting for the next signal
    continue = waitForReply cancel checkSignal

    -- Send a FIND_VALUE command, looking for the supplied id
    sendS = sendSignal (FIND_VALUE nid)

    -- As long as there still are pending requests, wait for the next one
    finish = do
        pending <- gets lookupStatePending
        unless (null pending) $ waitForReply (return ()) finishCheck

    -- Record the nodes which return the value
    finishCheck (Signal origin (RETURN_VALUE _ _)) = do
        known <- gets lookupStateKnown
        modify $ \s -> s { lookupStateKnown = origin:known }
        finish
    finishCheck _ = finish

--------------------------------------------------------------------------------

-- | Store assign a value to a key and store it in the DHT
store
  :: KademliaInstance -> Ident -> Value -> IO ()
store inst key val = runLookup go inst key
  where
    go = startLookup (instanceConfig inst) sendS end checkSignal

    -- Always add the nodes into the loop and continue the lookup
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) =
      continueLookup nodes sendS continue end

    checkSignal _ = error "Meet unknown signal in store"

    -- Continuing always means waiting for the next signal
    continue = waitForReply end checkSignal

    -- Send a FIND_NODE command, looking for the node corresponding to the key
    sendS = sendSignal (FIND_NODE key)

    -- Run the lookup as long as possible, to make sure the nodes closest
    -- to the key were polled.
    end = do
      polled <- gets lookupStatePolled

      unless (null polled) $ do
        let h = instanceHandle inst
        let k' = configK $ instanceConfig inst
        -- Don't select more than k peers
        let peerNum = if length polled > k' then k' else length polled
        -- Select the peers closest to the key
        let storePeers = map nodePeer
                         $ take peerNum
                         $ sortByDistanceTo key polled

        -- Send them a STORE command
        forM_ storePeers $ \storePeer -> do
          liftIO $ send h storePeer (STORE key val)

--------------------------------------------------------------------------------

-- | The different possible results of joinNetwork
data JoinResult
  = JoinSuccess
  | NodeDown
  | NodeBanned
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | Make a KademliaInstance join the network a supplied Node is in
joinNetwork
  :: KademliaInstance
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
      forM_ nodes $ \node -> do
        liftIO $ insertNode inst node
      continueLookup nodes sendS continue finish

    checkSignal _ = error "Unknow signal for @joinNetwork@"

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
  :: KademliaInstance
  -> Ident
  -> IO (Maybe Node)
lookupNode inst nid = runLookup go inst nid
  where
    go :: LookupM (Maybe Node)
    go = startLookup (instanceConfig inst) sendS end checkSignal

    -- Return empty list on lookup failure
    end :: LookupM (Maybe Node)
    end = pure Nothing

    -- Check whether the Node we are looking for was found.
    --
    -- There are two cases after receiving:
    -- 1. If we didn't find the node then continue lookup.
    -- 2. Otherwise return the found node.
    --
    -- Also insert all returned nodes to our tree.
    checkSignal :: Signal -> LookupM (Maybe Node)
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) = do
      forM_ nodes $ \node -> do
        liftIO $ insertNode inst node
      let targetNode = find ((== nid) . nodeId) nodes
      case targetNode of
        Nothing  -> continueLookup nodes sendS continue end
        justNode -> pure justNode
    checkSignal _ = end -- TODO: maybe it should be `panic` if we get some other return result

    -- Continuing always means waiting for the next signal
    continue :: LookupM (Maybe Node)
    continue = waitForReply end checkSignal

    -- Send a 'FIND_NODE' command looking for the node corresponding to
    -- the given 'Node'.
    sendS :: Node -> LookupM ()
    sendS = sendSignal (FIND_NODE nid)

----------------------------------------------------------------------------

-- | The state of a lookup.
data LookupState
  = LookupState
    { lookupStateInstance  :: !KademliaInstance
    , lookupStateTargetId  :: !Ident
    , lookupStateReplyChan :: !(Chan Reply)
    , lookupStateKnown     :: ![Node]
    , lookupStatePending   :: !(Map Node Word8)
    , lookupStatePolled    :: ![Node]
    }
  deriving ()

----------------------------------------------------------------------------

-- | Monad transformer for a lookup.
type LookupM = StateT LookupState IO

-- | Run a 'LookupM', returning its result.
runLookup
  :: LookupM a
  -> KademliaInstance
  -> Ident
  -> IO a
runLookup lookupM inst nid = do
  chan <- newChan
  let state = LookupState inst nid chan mempty mempty mempty
  evalStateT lookupM state

----------------------------------------------------------------------------

-- |
-- The initial phase of the normal Kademlia lookup operation
startLookup
  :: KademliaConfig
  -> (Node -> LookupM ())
  -> LookupM a
  -> (Signal -> LookupM a)
  -> LookupM a
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
  :: LookupM a
  -> (Signal -> LookupM a)
  -> LookupM a
waitForReply = waitForReplyDo False

--------------------------------------------------------------------------------

-- Wait for the next reply and handle it appropriately
waitForReplyDo
  :: Bool
  -> LookupM a
  -> (Signal -> LookupM a)
  -> LookupM a
waitForReplyDo withinJoin cancel onSignal = do
  chan <- gets lookupStateReplyChan
  inst <- gets lookupStateInstance

  let modifyPending f s = s { lookupStatePending = f (lookupStatePending s) }
  let modifyPolled  f s = s { lookupStatePolled  = f (lookupStatePolled  s) }
  let modifyKnown   f s = s { lookupStateKnown   = f (lookupStateKnown   s) }

  -- Remove the node from the list of nodes with pending replies
  let removeFromPending peer
        = modify (modifyPending (Map.delete peer))

  -- Remove every trace of the node's existance
  let removeFromEverywhere node
        = modify (modifyPending  (Map.delete node)
                  . modifyKnown  (delete node)
                  . modifyPolled (delete node))

  -- FIXME: doc
  let removeFromEverywherePeer peer
        = modify (modifyPending  (Map.filterWithKey
                                  (\k _ -> nodePeer k /= peer))
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
                   modify (modifyPending (Map.insert node 0))
                   modify (modifyPolled  (node:))

                 -- Insert the node into the tree, as it might be a new one or
                 -- it would have to be refreshed
                 liftIO $ insertNode inst node

                 case cmd of
                   RETURN_NODES n nid _ -> do
                     -- When a RETURN_NODES message with count @n@ is received,
                     -- this logic will increment a counter (in the @pending@
                     -- map) by 1 until that counter is equal to @n@, at which
                     -- point the counter will be removed from the @pending@
                     -- map. This is pretty naive and should be overhauled.
                     --
                     -- TODO: this logic should account for UDP packet loss
                     --
                     -- Almost everything in this codebase is trivially DoSable;
                     -- we should mitigate this using the cryptopuzzles defined
                     -- in the S-Kademlia paper, which add a proof of work for
                     -- each message received.
                     toRemove <- maybe True (\s -> s + 1 >= n)
                                 <$> gets (Map.lookup node . lookupStatePending)
                     if toRemove
                       then removeFromPending node
                       else do modify (modifyPending (Map.adjust (+1) node))
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
  :: [Node]
  -> (Node -> LookupM ())
  -> LookupM a
  -> LookupM a
  -> LookupM a
continueLookup nodes signalAction continue end = do
  let closest
        :: KademliaInstance
        -> [Node]
        -> LookupM [Node]
      closest inst known = do
        cid    <- gets lookupStateTargetId
        polled <- gets lookupStatePolled
        let cfg = instanceConfig inst

        -- Return the k closest nodes, the lookup had contact with
        pure (take
              (configK cfg)
              (sortByDistanceTo cid (known ++ polled)))

  let allClosestPolled
        :: KademliaInstance
        -> [Node]
        -> LookupM Bool
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
                 $ sortByDistanceTo nid
                 $ filter (`notElem` polled) (nodes ++ known)

  -- Check if k closest nodes have been polled already
  polledNeighbours <- allClosestPolled inst newKnown

  if | not (null newKnown) && not polledNeighbours -> do
         -- Send signal to the closest node that hasn't been polled yet
         let next = head (sortByDistanceTo nid newKnown)
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
  :: Command
  -> Peer
  -> LookupM ()
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
          FIND_NODE nid  -> ReplyRegistration [R_RETURN_NODES nid] peer
          FIND_VALUE nid -> ReplyRegistration
                            [R_RETURN_NODES nid, R_RETURN_VALUE nid] peer
          _              -> error "Unknown command at @sendSignal@"

    -- Expect an appropriate reply to the command
    liftIO $ expect h regs chan

--------------------------------------------------------------------------------

-- |
-- Send a signal to a node
sendSignal
  :: Command
  -> Node
  -> LookupM ()
sendSignal cmd node = do
  inst <- gets lookupStateInstance

  unlessM (liftIO (isNodeBanned inst (nodePeer node))) $ do
    sendSignalWithoutPolled cmd (nodePeer node)
    polled  <- gets lookupStatePolled
    pending <- gets lookupStatePending
    -- Mark the node as polled and pending
    modify $ \s -> s { lookupStatePolled  = node : polled
                     , lookupStatePending = Map.insert node 0 pending
                     }

--------------------------------------------------------------------------------

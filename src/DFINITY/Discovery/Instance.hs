--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Instance
-- Description: Implementation of the 'KademliaInstance' type
--
-- "DFINITY.Discovery.Instance" implements the 'KademliaInstance' type.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Instance
  ( KademliaInstance (..)
  , KademliaState (..)
  , BanState (..)
  , KademliaSnapshot (..)
  , defaultConfig
  , newInstance
  , insertNode
  , lookupNode
  , lookupNodeByPeer
  , insertValue
  , deleteValue
  , lookupValue
  , dumpPeers
  , banNode
  , isNodeBanned
  , takeSnapshot
  , takeSnapshot'
  , restoreInstance
  , viewBuckets
  , peersToNodeIds
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow                (second)
import           Control.Concurrent           (ThreadId)
import qualified Control.Concurrent.STM       as STM
import           Control.Monad                (unless)
import           Data.Time.Clock.POSIX        (POSIXTime, getPOSIXTime)
import           GHC.Generics                 (Generic)

import           Data.Map.Lazy                (Map)
import qualified Data.Map.Lazy                as Map

import           Data.IP                      (IP)
import           Network.Socket               (PortNumber)

import           DFINITY.Discovery.Config
                 (KademliaConfig, defaultConfig, usingConfig)
import           DFINITY.Discovery.Networking (KademliaHandle)
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Ident, Node (Node, nodePeer), Peer (Peer), Timestamp, Value)

--------------------------------------------------------------------------------

-- |
-- The handle of a running Kademlia node.
data KademliaInstance
  = KademliaInstance
    { instanceNode              :: !Node
      -- ^ FIXME: doc
    , instanceHandle            :: !KademliaHandle
      -- ^ FIXME: doc
    , instanceState             :: !KademliaState
      -- ^ FIXME: doc
    , instanceExpirationThreads :: !(STM.TVar (Map Ident ThreadId))
      -- ^ FIXME: doc
    , instanceConfig            :: !KademliaConfig
      -- ^ FIXME: doc
    }
  deriving ()

--------------------------------------------------------------------------------

-- |
-- The ban state for some node.
data BanState
  = -- | The node is permanently banned.
    BanForever
  | -- | The node is banned until the specified date/time.
    BanTill
    !POSIXTime
    -- ^ The time at which the ban ends.
  | -- | The node is not banned.
    NoBan
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------

-- |
-- Representation of the data the 'KademliaProcess' carries.
data KademliaState
  = KademliaState
    { stateTree   :: !(STM.TVar T.NodeTree)
      -- ^ FIXME: doc
    , stateBanned :: !(STM.TVar (Map Peer BanState))
      -- ^ FIXME: doc
    , stateValues :: !(STM.TVar (Map Ident Value))
      -- ^ FIXME: doc
    }
  deriving ()

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
data KademliaSnapshot
  = KademliaSnapshot
    { snapshotTree   :: !T.NodeTree
      -- ^ FIXME: doc
    , snapshotBanned :: !(Map Peer BanState)
      -- ^ FIXME: doc
    , snapshotValues :: !(Map Ident Value)
      -- ^ FIXME: doc
    }
  deriving (Generic)

--------------------------------------------------------------------------------

-- |
-- Create a new 'KademliaInstance' from an ID and a 'KademliaHandle'.
newInstance
  :: Ident
  -> (IP, PortNumber)
  -> KademliaConfig
  -> KademliaHandle
  -> IO KademliaInstance
newInstance nid (extHost, extPort) cfg handle = do
  tree    <- STM.atomically $ STM.newTVar (T.create nid)
  banned  <- STM.atomically $ STM.newTVar Map.empty
  threads <- STM.atomically $ STM.newTVar Map.empty
  values  <- STM.atomically $ STM.newTVar Map.empty
  let ownNode = Node (Peer extHost extPort) nid
  let state = KademliaState tree banned values
  pure $ KademliaInstance ownNode handle state threads cfg

--------------------------------------------------------------------------------

-- |
-- Insert a 'Node' into the 'NodeTree'.
insertNode
  :: KademliaInstance
  -> Node
  -> IO ()
insertNode inst node = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _ _) = state
  currentTime <- floor <$> getPOSIXTime
  isBanned <- isNodeBanned inst (nodePeer node)
  unless isBanned $ do
    STM.atomically $ do
      tree <- STM.readTVar sTree
      STM.writeTVar sTree $ T.insert tree node currentTime `usingConfig` cfg

--------------------------------------------------------------------------------

-- |
-- Lookup a 'Node' in the 'NodeTree'.
lookupNode
  :: KademliaInstance
  -> Ident
  -> IO (Maybe Node)
lookupNode inst nid = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _ _) = state
  tree <- STM.atomically (STM.readTVar sTree)
  pure $ T.lookup tree nid `usingConfig` cfg

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
lookupNodeByPeer
  :: KademliaInstance
  -> Peer
  -> IO (Maybe Node)
lookupNodeByPeer inst peer = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _ _) = state
  tree <- STM.atomically (STM.readTVar sTree)
  pure (do nid <- Map.lookup peer (T.nodeTreePeers tree)
           T.lookup tree nid `usingConfig` cfg)

--------------------------------------------------------------------------------

-- |
-- Return all the Nodes an Instance has encountered so far
dumpPeers
  :: KademliaInstance
  -> IO [(Node, Timestamp)]
dumpPeers inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _ _) = state
  currentTime <- floor <$> getPOSIXTime
  STM.atomically $ do
    tree <- STM.readTVar sTree
    pure $ map (second (\t -> currentTime - t)) (T.toList tree)

--------------------------------------------------------------------------------

-- |
-- Insert a value into the store.
insertValue
  :: Ident
  -> Value
  -> KademliaInstance
  -> IO ()
insertValue key value inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ values) = state
  STM.atomically $ do
    STM.modifyTVar' values (Map.insert key value)

--------------------------------------------------------------------------------

-- |
-- Delete a value from the store.
deleteValue
  :: Ident
  -> KademliaInstance
  -> IO ()
deleteValue key inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ values) = state
  STM.atomically $ do
    STM.modifyTVar' values (Map.delete key)

--------------------------------------------------------------------------------

-- |
-- Lookup a value in the store.
lookupValue
  :: Ident
  -> KademliaInstance
  -> IO (Maybe Value)
lookupValue key inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ values) = state
  STM.atomically $ do
    Map.lookup key <$> STM.readTVar values

--------------------------------------------------------------------------------

-- |
-- Check whether node is banned
isNodeBanned
  :: KademliaInstance
  -> Peer
  -> IO Bool
isNodeBanned inst peer = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ banned _) = state

  let isBanned NoBan         = pure False
      isBanned BanForever    = pure True
      isBanned (BanTill end) = do cur <- getPOSIXTime
                                  pure (cur < end)

  banSet <- STM.atomically $ STM.readTVar banned
  case Map.lookup peer banSet of
    Nothing -> pure False
    Just b  -> do stillBanned <- isBanned b
                  unless stillBanned $ STM.atomically $ do
                    STM.modifyTVar banned $ Map.delete peer
                  pure stillBanned

--------------------------------------------------------------------------------

-- |
-- Mark node as banned
banNode
  :: KademliaInstance
  -> Node
  -> BanState
  -> IO ()
banNode inst node ban = STM.atomically $ do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree banned _) = state
  STM.modifyTVar banned $ Map.insert (nodePeer node) ban
  STM.modifyTVar sTree $ \t -> T.delete t (nodePeer node) `usingConfig` cfg

--------------------------------------------------------------------------------

-- |
-- Take a snapshot of the given 'KademliaState'.
takeSnapshot'
  :: KademliaState
  -> IO KademliaSnapshot
takeSnapshot' (KademliaState tree banned values) = do
  STM.atomically $ do
    snapshotTree   <- STM.readTVar tree
    snapshotBanned <- STM.readTVar banned
    snapshotValues <- STM.readTVar values
    pure (KademliaSnapshot {..})

--------------------------------------------------------------------------------

-- |
-- Take a snapshot of the 'KademliaState' for the given 'KademliaInstance'.
takeSnapshot
  :: KademliaInstance
  -> IO KademliaSnapshot
takeSnapshot = takeSnapshot' . instanceState

--------------------------------------------------------------------------------

-- |
-- Restores instance from snapshot.
restoreInstance
  :: (IP, PortNumber)
  -> KademliaConfig
  -> KademliaHandle
  -> KademliaSnapshot
  -> IO KademliaInstance
restoreInstance extAddr cfg handle snapshot = do
  let nid = T.extractId (snapshotTree snapshot)
  let emptyInstance = newInstance nid extAddr cfg handle
  inst <- emptyInstance
  let st = instanceState inst
  STM.atomically $ do
    STM.writeTVar (stateTree st) (snapshotTree snapshot)
  STM.atomically $ do
    STM.writeTVar (stateBanned st) (snapshotBanned snapshot)
  pure inst

--------------------------------------------------------------------------------

-- |
-- Shows stored buckets, ordered by distance to this node
viewBuckets
  :: KademliaInstance
  -> IO [[(Node, Timestamp)]]
viewBuckets inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _ _) = state
  currentTime <- floor <$> getPOSIXTime
  map (map (second (currentTime -))) <$> T.toView <$> STM.readTVarIO sTree

--------------------------------------------------------------------------------

peersToNodeIds
  :: KademliaInstance
  -> [Peer]
  -> IO [Maybe Node]
peersToNodeIds inst peers = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _ _) = state
  knownPeers <- T.nodeTreePeers <$> STM.atomically (STM.readTVar sTree)
  pure $ zipWith (fmap . Node) peers $ map (`Map.lookup` knownPeers) peers

--------------------------------------------------------------------------------

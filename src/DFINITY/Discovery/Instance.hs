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
import           Data.Word                    (Word16)
import           GHC.Generics                 (Generic)

import           Data.Map.Lazy                (Map)
import qualified Data.Map.Lazy                as Map

import           Data.Text                    (Text)

import           DFINITY.Discovery.Config
                 (KademliaConfig, defaultConfig, usingConfig)
import           DFINITY.Discovery.Networking (KademliaHandle)
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Node (Node, nodePeer), Peer (Peer), Serialize, Timestamp)

--------------------------------------------------------------------------------

-- |
-- The handle of a running Kademlia node.
data KademliaInstance i a
  = KademliaInstance
    { instanceNode              :: !(Node i)
      -- ^ FIXME: doc
    , instanceHandle            :: !(KademliaHandle i a)
      -- ^ FIXME: doc
    , instanceState             :: !(KademliaState i a)
      -- ^ FIXME: doc
    , instanceExpirationThreads :: !(STM.TVar (Map i ThreadId))
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
data KademliaState i a
  = KademliaState
    { stateTree   :: !(TVar (T.NodeTree i))
      -- ^ FIXME: doc
    , stateBanned :: !(TVar (Map Peer BanState))
      -- ^ FIXME: doc
    , stateValues :: !(Maybe (TVar (Map i a)))
      -- ^ FIXME: doc
    }
  deriving ()

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
data KademliaSnapshot i
  = KademliaSnapshot
    { snapshotTree   :: !(T.NodeTree i)
      -- ^ FIXME: doc
    , snapshotBanned :: !(Map Peer BanState)
      -- ^ FIXME: doc
    }
  deriving (Generic)

--------------------------------------------------------------------------------

-- |
-- Create a new 'KademliaInstance' from an ID and a 'KademliaHandle'.
newInstance
  :: (Serialize i)
  => i
  -> (Text, Word16)
  -> KademliaConfig
  -> KademliaHandle i a
  -> IO (KademliaInstance i a)
newInstance nid (extHost, extPort) cfg handle = do
  tree    <- STM.atomically $ STM.newTVar (T.create nid `usingConfig` cfg)
  banned  <- STM.atomically $ STM.newTVar Map.empty
  threads <- STM.atomically $ STM.newTVar Map.empty
  let ownNode = Node (Peer extHost (fromIntegral extPort)) nid
  pure $ KademliaInstance ownNode handle (KademliaState tree banned) threads cfg

--------------------------------------------------------------------------------

-- |
-- Insert a 'Node' into the 'NodeTree'.
insertNode
  :: (Serialize i, Ord i)
  => KademliaInstance i a
  -> Node i
  -> IO ()
insertNode inst node = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _) = state
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
  :: (Serialize i, Ord i)
  => KademliaInstance i a
  -> i
  -> IO (Maybe (Node i))
lookupNode inst nid = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _) = state
  tree <- STM.atomically (STM.readTVar sTree)
  pure $ T.lookup tree nid `usingConfig` cfg

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
lookupNodeByPeer
  :: (Serialize i, Ord i)
  => KademliaInstance i a
  -> Peer
  -> IO (Maybe (Node i))
lookupNodeByPeer inst peer = do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree _) = state
  tree <- STM.atomically (STM.readTVar sTree)
  pure (do nid <- Map.lookup peer (T.nodeTreePeers tree)
           T.lookup tree nid `usingConfig` cfg)

--------------------------------------------------------------------------------

-- |
-- Return all the Nodes an Instance has encountered so far
dumpPeers
  :: KademliaInstance i a
  -> IO [(Node i, Timestamp)]
dumpPeers inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _) = state
  currentTime <- floor <$> getPOSIXTime
  STM.atomically $ do
    tree <- STM.readTVar sTree
    pure $ map (second (\t -> currentTime - t)) (T.toList tree)

--------------------------------------------------------------------------------

-- |
-- Insert a value into the store.
insertValue
  :: (Ord i)
  => i
  -> a
  -> KademliaInstance i a
  -> IO ()
insertValue key value inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ maybeValues) = state
  case maybeValues of
    Just values -> do atomically $ do
                        vals <- readTVar values
                        writeTVar values (Map.insert key value vals)
    Nothing     -> pure ()

--------------------------------------------------------------------------------

-- |
-- Delete a value from the store.
deleteValue
  :: (Ord i)
  => i
  -> KademliaInstance i a
  -> IO ()
deleteValue key inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ maybeValues) = state
  case maybeValues of
    Just values -> do atomically $ do
                        vals <- readTVar values
                        writeTVar values (Map.delete key vals)
    Nothing     -> pure ()

--------------------------------------------------------------------------------

-- |
-- Lookup a value in the store.
lookupValue
  :: (Ord i)
  => i
  -> KademliaInstance i a
  -> IO (Maybe a)
lookupValue key inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ _ maybeValues) = state
  case maybeValues of
    Just values -> do atomically $ do
                        vals <- readTVar values
                        pure (Map.lookup key vals)
    Nothing     -> pure Nothing

--------------------------------------------------------------------------------

-- |
-- Check whether node is banned
isNodeBanned
  :: (Ord i)
  => KademliaInstance i a
  -> Peer
  -> IO Bool
isNodeBanned inst peer = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState _ banned) = state

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
  :: (Serialize i, Ord i)
  => KademliaInstance i a
  -> Node i
  -> BanState
  -> IO ()
banNode inst node ban = STM.atomically $ do
  let (KademliaInstance _ _ state _ cfg) = inst
  let (KademliaState sTree banned) = state
  STM.modifyTVar banned $ Map.insert (nodePeer node) ban
  STM.modifyTVar sTree $ \t -> T.delete t (nodePeer node) `usingConfig` cfg

--------------------------------------------------------------------------------

-- |
-- Take a snapshot of the given 'KademliaState'.
takeSnapshot'
  :: KademliaState i a
  -> IO (KademliaSnapshot i)
takeSnapshot' (KademliaState tree banned) = do
  STM.atomically $ do
    snapshotTree   <- STM.readTVar tree
    snapshotBanned <- STM.readTVar banned
    pure (KademliaSnapshot {..})

--------------------------------------------------------------------------------

-- |
-- Take a snapshot of the 'KademliaState' for the given 'KademliaInstance'.
takeSnapshot
  :: KademliaInstance i a
  -> IO (KademliaSnapshot i)
takeSnapshot = takeSnapshot' . instanceState

--------------------------------------------------------------------------------

-- |
-- Restores instance from snapshot.
restoreInstance
  :: (Serialize i)
  => (Text, Word16)
  -> KademliaConfig
  -> KademliaHandle i a
  -> KademliaSnapshot i
  -> IO (KademliaInstance i a)
restoreInstance extAddr cfg handle snapshot = do
  let nid = T.extractId (snapshotTree snapshot) `usingConfig` cfg
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
  :: KademliaInstance i a
  -> IO [[(Node i, Timestamp)]]
viewBuckets inst = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _) = state
  currentTime <- floor <$> getPOSIXTime
  map (map (second (currentTime -))) <$> T.toView <$> STM.readTVarIO sTree

--------------------------------------------------------------------------------

peersToNodeIds
  :: KademliaInstance i a
  -> [Peer]
  -> IO [Maybe (Node i)]
peersToNodeIds inst peers = do
  let (KademliaInstance _ _ state _ _) = inst
  let (KademliaState sTree _) = state
  knownPeers <- T.nodeTreePeers <$> STM.atomically (STM.readTVar sTree)
  pure $ zipWith (fmap . Node) peers $ map (`Map.lookup` knownPeers) peers

--------------------------------------------------------------------------------

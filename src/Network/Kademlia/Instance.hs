{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type.
-}

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Kademlia.Instance
    ( KademliaInstance (..)
    , KademliaState    (..)
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

import           Control.Arrow               (second)
import           Control.Concurrent          (ThreadId)
import           Control.Concurrent.STM
                 (TVar, atomically, modifyTVar, newTVar, readTVar, readTVarIO,
                 writeTVar)
import           Control.Monad               (unless)
import           Control.Monad.Extra         (unlessM)
import           Control.Monad.Trans         ()
import           Control.Monad.Trans.Reader  ()
import           Control.Monad.Trans.State   ()
import           Data.Map                    (Map)
import qualified Data.Map                    as M hiding (Map)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Word                   (Word16)
import           GHC.Generics                (Generic)

import           Network.Kademlia.Config
                 (KademliaConfig (configStoreValues), defaultConfig,
                 usingConfig)
import           Network.Kademlia.Networking (KademliaHandle (..))
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types
                 (Node (..), Peer (..), Serialize (..), Timestamp)

-- | The handle of a running Kademlia Node
data KademliaInstance i a
  = KademliaInstance
    { instanceNode              :: Node i
    , instanceHandle            :: KademliaHandle i a
    , instanceState             :: KademliaState i a
    , instanceExpirationThreads :: TVar (Map i ThreadId)
    , instanceConfig            :: KademliaConfig
    }
  deriving ()

-- | Ban condition for some node
data BanState
  = BanForever
  | BanTill Integer  -- time in microseconds
  | NoBan
  deriving (Eq, Show, Generic)

-- | Representation of the data the KademliaProcess carries
data KademliaState i a
  = KademliaState
    { stateTree   :: TVar (T.NodeTree i)
    , stateBanned :: TVar (Map Peer BanState)
    , stateValues :: Maybe (TVar (Map i a))
    }
  deriving ()

data KademliaSnapshot i
  = KademliaSnapshot
    { snapshotTree   :: T.NodeTree i
    , snapshotBanned :: Map Peer BanState
    }
  deriving (Generic)

-- | Create a new KademliaInstance from an Id and a KademliaHandle
newInstance
    :: Serialize i
    => i -> (String, Word16) -> KademliaConfig -> KademliaHandle i a -> IO (KademliaInstance i a)
newInstance nid (extHost, extPort) cfg handle = do
    tree <- atomically $ newTVar (T.create nid `usingConfig` cfg)
    banned <- atomically . newTVar $ M.empty
    values <- if configStoreValues cfg then Just <$> (atomically . newTVar $ M.empty) else pure Nothing
    threads <- atomically . newTVar $ M.empty
    let ownNode = Node (Peer extHost $ fromIntegral extPort) nid
    return $ KademliaInstance ownNode handle (KademliaState tree banned values) threads cfg

-- | Insert a Node into the NodeTree
insertNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> IO ()
insertNode inst@(KademliaInstance _ _ (KademliaState sTree _ _) _ cfg) node = do
    currentTime <- floor <$> getPOSIXTime
    unlessM (isNodeBanned inst $ nodePeer node) $ atomically $ do
        tree <- readTVar sTree
        writeTVar sTree $ T.insert tree node currentTime `usingConfig` cfg

-- | Lookup a Node in the NodeTree
lookupNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode (KademliaInstance _ _ (KademliaState sTree _ _) _ cfg) nid = do
    tree <- atomically $ readTVar sTree
    pure $ T.lookup tree nid `usingConfig` cfg

lookupNodeByPeer :: (Serialize i, Ord i) => KademliaInstance i a -> Peer -> IO (Maybe (Node i))
lookupNodeByPeer (KademliaInstance _ _ (KademliaState sTree _ _) _ cfg) peer = do
    tree <- atomically (readTVar sTree)
    pure $
        case M.lookup peer (T.nodeTreePeers tree) of
            Nothing  -> Nothing
            Just nid -> T.lookup tree nid `usingConfig` cfg

-- | Return all the Nodes an Instance has encountered so far
dumpPeers :: KademliaInstance i a -> IO [(Node i, Timestamp)]
dumpPeers (KademliaInstance _ _ (KademliaState sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    atomically $ do
        tree <- readTVar sTree
        return . map (second (currentTime -)) . T.toList $ tree

-- | Insert a value into the store
insertValue :: (Ord i) => i -> a -> KademliaInstance i a -> IO ()
insertValue _ _ (KademliaInstance _ _ (KademliaState _ _ Nothing) _ _)             = return ()
insertValue key value (KademliaInstance _ _ (KademliaState _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.insert key value vals

-- | Delete a value from the store
deleteValue :: (Ord i) => i -> KademliaInstance i a -> IO ()
deleteValue _ (KademliaInstance _ _ (KademliaState _ _ Nothing) _ _)         = return ()
deleteValue key (KademliaInstance _ _ (KademliaState _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.delete key vals

-- | Lookup a value in the store
lookupValue :: (Ord i) => i -> KademliaInstance i a -> IO (Maybe a)
lookupValue _   (KademliaInstance _ _ (KademliaState _ _ Nothing) _ _) = pure Nothing
lookupValue key (KademliaInstance _ _ (KademliaState _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    return . M.lookup key $ vals

-- | Check whether node is banned
isNodeBanned :: Ord i => KademliaInstance i a -> Peer -> IO Bool
isNodeBanned (KademliaInstance _ _ (KademliaState _ banned _) _ _) pr = do
    banSet <- atomically $ readTVar banned
    case M.lookup pr banSet of
        Nothing -> pure False
        Just b  -> do
            stillBanned <- isBanned b
            unless stillBanned $ atomically . modifyTVar banned $ M.delete pr
            pure stillBanned
  where
    isBanned NoBan       = pure False
    isBanned BanForever  = pure True
    isBanned (BanTill t) = ( < t) . round <$> getPOSIXTime

-- | Mark node as banned
banNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> BanState -> IO ()
banNode (KademliaInstance _ _ (KademliaState sTree banned _) _ cfg) node ban = atomically $ do
    modifyTVar banned $ M.insert (nodePeer node) ban
    modifyTVar sTree $ \t -> T.delete t (nodePeer node) `usingConfig` cfg

-- | Take a current view of `KademliaState`.
takeSnapshot' :: KademliaState i a -> IO (KademliaSnapshot i)
takeSnapshot' (KademliaState tree banned _) = atomically $ do
    snapshotTree   <- readTVar tree
    snapshotBanned <- readTVar banned
    return (KademliaSnapshot {..})

-- | Take a current view of `KademliaState`.
takeSnapshot :: KademliaInstance i a -> IO (KademliaSnapshot i)
takeSnapshot = takeSnapshot' . instanceState

-- | Restores instance from snapshot.
restoreInstance :: Serialize i => (String, Word16) -> KademliaConfig -> KademliaHandle i a
                -> KademliaSnapshot i -> IO (KademliaInstance i a)
restoreInstance extAddr cfg handle snapshot = do
    inst <- emptyInstance
    let st = instanceState inst
    atomically . writeTVar (stateTree   st) $ snapshotTree   snapshot
    atomically . writeTVar (stateBanned st) $ snapshotBanned snapshot
    return inst
  where
    emptyInstance = newInstance nid extAddr cfg handle
    nid           = T.extractId (snapshotTree snapshot) `usingConfig` cfg

-- | Shows stored buckets, ordered by distance to this node
viewBuckets :: KademliaInstance i a -> IO [[(Node i, Timestamp)]]
viewBuckets (KademliaInstance _ _ (KademliaState sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    map (map $ second (currentTime -)) <$> T.toView <$> readTVarIO sTree

peersToNodeIds :: KademliaInstance i a -> [Peer] -> IO [Maybe (Node i)]
peersToNodeIds (KademliaInstance _ _ (KademliaState sTree _ _) _ _) peers = do
    knownPeers <- T.nodeTreePeers <$> atomically (readTVar sTree)
    pure $ zipWith (fmap . Node) peers $ map (`M.lookup` knownPeers) peers

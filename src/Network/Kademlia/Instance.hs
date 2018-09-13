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
                 (KademliaConfig, defaultConfig, storeValues, usingConfig)
import           Network.Kademlia.Networking (KademliaHandle (..))
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types
                 (Node (..), Peer (..), Serialize (..), Timestamp)

-- | The handle of a running Kademlia Node
data KademliaInstance i a
    = KI {
      node              :: Node i
    , handle            :: KademliaHandle i a
    , state             :: KademliaState i a
    , expirationThreads :: TVar (Map i ThreadId)
    , config            :: KademliaConfig
    }

-- | Ban condition for some node
data BanState
    = BanForever
    | BanTill Integer  -- time in microseconds
    | NoBan
    deriving (Eq, Show, Generic)

-- | Representation of the data the KademliaProcess carries
data KademliaState i a
    = KS {
      sTree  :: TVar (T.NodeTree i)
    , banned :: TVar (Map Peer BanState)
    , values :: Maybe (TVar (Map i a))
    }

data KademliaSnapshot i
    = KSP {
      spTree   :: T.NodeTree i
    , spBanned :: Map Peer BanState
    } deriving (Generic)

-- | Create a new KademliaInstance from an Id and a KademliaHandle
newInstance
    :: Serialize i
    => i -> (String, Word16) -> KademliaConfig -> KademliaHandle i a -> IO (KademliaInstance i a)
newInstance nid (extHost, extPort) cfg handle = do
    tree <- atomically $ newTVar (T.create nid `usingConfig` cfg)
    banned <- atomically . newTVar $ M.empty
    values <- if storeValues cfg then Just <$> (atomically . newTVar $ M.empty) else pure Nothing
    threads <- atomically . newTVar $ M.empty
    let ownNode = Node (Peer extHost $ fromIntegral extPort) nid
    return $ KI ownNode handle (KS tree banned values) threads cfg

-- | Insert a Node into the NodeTree
insertNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> IO ()
insertNode inst@(KI _ _ (KS sTree _ _) _ cfg) node = do
    currentTime <- floor <$> getPOSIXTime
    unlessM (isNodeBanned inst $ peer node) $ atomically $ do
        tree <- readTVar sTree
        writeTVar sTree $ T.insert tree node currentTime `usingConfig` cfg

-- | Lookup a Node in the NodeTree
lookupNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode (KI _ _ (KS sTree _ _) _ cfg) nid = do
    tree <- atomically $ readTVar sTree
    pure $ T.lookup tree nid `usingConfig` cfg

lookupNodeByPeer :: (Serialize i, Ord i) => KademliaInstance i a -> Peer -> IO (Maybe (Node i))
lookupNodeByPeer (KI _ _ (KS sTree _ _) _ cfg) peer = do
    tree <- atomically (readTVar sTree)
    pure $
        case M.lookup peer (T.ntPeers tree) of
            Nothing  -> Nothing
            Just nid -> T.lookup tree nid `usingConfig` cfg

-- | Return all the Nodes an Instance has encountered so far
dumpPeers :: KademliaInstance i a -> IO [(Node i, Timestamp)]
dumpPeers (KI _ _ (KS sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    atomically $ do
        tree <- readTVar sTree
        return . map (second (currentTime -)) . T.toList $ tree

-- | Insert a value into the store
insertValue :: (Ord i) => i -> a -> KademliaInstance i a -> IO ()
insertValue _ _ (KI _ _ (KS _ _ Nothing) _ _)             = return ()
insertValue key value (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.insert key value vals

-- | Delete a value from the store
deleteValue :: (Ord i) => i -> KademliaInstance i a -> IO ()
deleteValue _ (KI _ _ (KS _ _ Nothing) _ _)         = return ()
deleteValue key (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.delete key vals

-- | Lookup a value in the store
lookupValue :: (Ord i) => i -> KademliaInstance i a -> IO (Maybe a)
lookupValue _   (KI _ _ (KS _ _ Nothing) _ _) = pure Nothing
lookupValue key (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    return . M.lookup key $ vals

-- | Check whether node is banned
isNodeBanned :: Ord i => KademliaInstance i a -> Peer -> IO Bool
isNodeBanned (KI _ _ (KS _ banned _) _ _) pr = do
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
banNode (KI _ _ (KS sTree banned _) _ cfg) node ban = atomically $ do
    modifyTVar banned $ M.insert (peer node) ban
    modifyTVar sTree $ \t -> T.delete t (peer node) `usingConfig` cfg

-- | Take a current view of `KademliaState`.
takeSnapshot' :: KademliaState i a -> IO (KademliaSnapshot i)
takeSnapshot' (KS tree banned _) = atomically $ do
    spTree   <- readTVar tree
    spBanned <- readTVar banned
    return KSP{..}

-- | Take a current view of `KademliaState`.
takeSnapshot :: KademliaInstance i a -> IO (KademliaSnapshot i)
takeSnapshot = takeSnapshot' . state

-- | Restores instance from snapshot.
restoreInstance :: Serialize i => (String, Word16) -> KademliaConfig -> KademliaHandle i a
                -> KademliaSnapshot i -> IO (KademliaInstance i a)
restoreInstance extAddr cfg handle snapshot = do
    inst <- emptyInstance
    let st = state inst
    atomically . writeTVar (sTree  st) $ spTree snapshot
    atomically . writeTVar (banned st) $ spBanned snapshot
    return inst
  where
    emptyInstance = newInstance nid extAddr cfg handle
    nid           = T.extractId (spTree snapshot) `usingConfig` cfg

-- | Shows stored buckets, ordered by distance to this node
viewBuckets :: KademliaInstance i a -> IO [[(Node i, Timestamp)]]
viewBuckets (KI _ _ (KS sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    map (map $ second (currentTime -)) <$> T.toView <$> readTVarIO sTree

peersToNodeIds :: KademliaInstance i a -> [Peer] -> IO [Maybe (Node i)]
peersToNodeIds (KI _ _ (KS sTree _ _) _ _) peers = do
    knownPeers <- T.ntPeers <$> atomically (readTVar sTree)
    pure $ zipWith (fmap . Node) peers $ map (`M.lookup` knownPeers) peers

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Kademlia.Config
       ( KademliaConfig(..)
       , WithConfigT (..)
       , WithConfig
       , getConfig
       , usingConfigT
       , usingConfig
       , defaultConfig
       , defaultRoutingSharingN
       , defaultK
       ) where

import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.Reader   (ReaderT (..), ask)
import           Control.Monad.Trans    (MonadTrans)
import           Network.Kademlia.Utils (hour, minute)

data KademliaConfig
  = KademliaConfig
    {
      configK               :: !Int  -- ^ @k@ nearest heighbours for query. Constant from paper.
    , configExpirationTime  :: !Int  -- ^ we delete a value after @expirationTime@ seconds has passed
    , configStoreValueTime  :: !Int  -- ^ we store all values stored in the node in the 'k' closest known nodes every @storeValueTime@ seconds
    , configPingTime        :: !Int  -- ^ we ping all known nodes every @pingTime@ seconds to make sure they are still present
    , configNumLookupNodes  :: !Int  -- ^ number of nodes to look in parallel during a lookup
                                     --   also known as α in kademlia paper
    , configMsgSizeLimit    :: !Int  -- ^ upper bound on size of message transfered through
                                     --   network; exceeding messages would be splitted
    , configStoreValues     :: !Bool -- ^ when this is False, we don't store anything in this node
    , configRoutingSharingN :: !Int  -- ^ number of nodes from not closest to include int `returnNodes` responses (see [CSL-260])
    , configCacheSize       :: !Int  -- ^ cache size used by Node Storage Tree
    , configPingLimit       :: !Int  -- ^ after unsuccessfull @pingLimit@ pings node will be throwed out from bucket
    }

newtype WithConfigT m a = WithConfigT
     { getWithConfigT :: ReaderT KademliaConfig m a
     } deriving (Functor, Applicative, Monad, MonadTrans)

type WithConfig a = WithConfigT Identity a

getConfig :: Monad m => WithConfigT m KademliaConfig
getConfig = WithConfigT ask

usingConfigT :: WithConfigT m a -> KademliaConfig -> m a
usingConfigT f cfg = flip runReaderT cfg $ getWithConfigT f

usingConfig :: WithConfig a -> KademliaConfig -> a
usingConfig f cfg = runIdentity $ usingConfigT f cfg

defaultK :: Int
defaultK = 7

defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ defaultK `divMod` 2

defaultConfig :: KademliaConfig
defaultConfig = KademliaConfig
    { configK               = defaultK
    , configExpirationTime  = hour 1
    , configStoreValueTime  = hour 1
    , configPingTime        = minute 5
    , configNumLookupNodes  = 3
    , configMsgSizeLimit    = 1200
    , configStoreValues     = True
    , configRoutingSharingN = defaultRoutingSharingN
    , configCacheSize       = 5
    , configPingLimit       = 4
    }

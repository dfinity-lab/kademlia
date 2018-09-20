--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Network.Kademlia.Config
-- Description : Configuration parameters for a Kademlia instance.
--
-- FIXME: doc

--------------------------------------------------------------------------------

module Network.Kademlia.Config
  ( KademliaConfig (..)
  , WithConfigT (..)
  , WithConfig
  , getConfig
  , usingConfigT
  , usingConfig
  , defaultConfig
  , defaultRoutingSharingN
  , defaultK
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadTrans)
import           Network.Kademlia.Utils (minute)

--------------------------------------------------------------------------------

-- |
-- This type encompasses all configuration parameters for a running Kademlia
-- instance.
data KademliaConfig
  = KademliaConfig
    { configK               :: !Int
      -- ^ Queries use the @k@ nearest heighbours; this is that @k@.
      --   This is defined as a constant in the paper.
      --
      --   FIXME: this should be a type of positive numbers
    , configPingTime        :: !Int
      -- ^ This constant defines the period (in seconds) with which we ping all
      --   known nodes to make sure they are still present.
      --
      --   FIXME: this should have higher resolution than seconds.
      --
      --   FIXME: this should be a type of positive numbers
    , configNumLookupNodes  :: !Int
      -- ^ The number of nodes to look in parallel during a lookup; this is
      --   also known as α in the paper.
      --
      --   FIXME: this should be a type of positive numbers
    , configMsgSizeLimit    :: !Int
      -- ^ The upper bound on size of a message transfered through network;
      --   messages larger than this will be split.
      --
      --   FIXME: what are the units?
      --
      --   FIXME: this should be a type of positive numbers
    , configRoutingSharingN :: !Int
      -- ^ The number of nodes from not closest to include in 'RETURN_NODES'
      --   responses (see [CSL-260]).
      --
      --   FIXME: rewrite this doc comment
    , configCacheSize       :: !Int
      -- ^ The cache size used by node storage tree.
      --
      --   FIXME: what are the units?
      --
      --   FIXME: this should be a type of nonnegative numbers
    , configPingLimit       :: !Int
      -- ^ The number of pings after which an unresponsive node will be thrown
      --   out from its bucket.
      --
      --   FIXME: this should be a type of positive numbers
    }
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

-- | FIXME: doc
defaultK :: Int
defaultK = 7

-- | FIXME: doc
defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ defaultK `divMod` 2

-- | FIXME: doc
defaultConfig :: KademliaConfig
defaultConfig
  = KademliaConfig
    { configK               = defaultK
    , configPingTime        = minute 5
    , configNumLookupNodes  = 3
    , configMsgSizeLimit    = 1200
    , configRoutingSharingN = defaultRoutingSharingN
    , configCacheSize       = 5
    , configPingLimit       = 4
    }

--------------------------------------------------------------------------------

-- |
-- A monad transformer that gives access to a 'KademliaConfig'.
newtype WithConfigT m a
  = WithConfigT
    { getWithConfigT :: ReaderT KademliaConfig m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

--------------------------------------------------------------------------------

-- | FIXME: doc
type WithConfig a = WithConfigT Identity a

--------------------------------------------------------------------------------

-- | FIXME: doc
getConfig :: Monad m => WithConfigT m KademliaConfig
getConfig = WithConfigT ask

-- | FIXME: doc
usingConfigT :: WithConfigT m a -> KademliaConfig -> m a
usingConfigT f cfg = flip runReaderT cfg $ getWithConfigT f

-- | FIXME: doc
usingConfig :: WithConfig a -> KademliaConfig -> a
usingConfig f cfg = runIdentity $ usingConfigT f cfg

--------------------------------------------------------------------------------

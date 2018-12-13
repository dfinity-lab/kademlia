--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Config
-- Description: Configuration parameters for a Kademlia instance.
--
-- FIXME: doc

--------------------------------------------------------------------------------

module DFINITY.Discovery.Config
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

import           Control.Monad.Identity      (Identity, runIdentity)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Control.Monad.Trans         (MonadTrans)
import           DFINITY.Discovery.Signature
                 (SignatureScheme, trivialSignatureScheme)
import           DFINITY.Discovery.Utils     (hour, minute)

--------------------------------------------------------------------------------

-- |
-- This type encompasses all configuration parameters for a running Kademlia
-- instance.
data KademliaConfig i
  = KademliaConfig
    { configK               :: !Int
      -- ^ Queries use the @k@ nearest heighbours; this is that @k@.
      --   This is defined as a constant in the paper.
      --
      --   FIXME: this should be a type of positive numbers
    , configExpirationTime  :: !Int
      -- ^ We delete a value after @configExpirationTime@ seconds has passed.
      --
      --   FIXME: this should have higher resolution than seconds.
      --
      --   FIXME: this should be a type of positive numbers
    , configStoreValueTime  :: !Int
      -- ^ We store all values stored in the node in the @k@ closest known nodes
      --   every @configStoreValueTime@ seconds.
      --
      --   FIXME: this should have higher resolution than seconds.
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
    , configSignatureScheme :: !(SignatureScheme IO i)
      -- ^ FIXME: doc
    }

--------------------------------------------------------------------------------

-- | FIXME: doc
defaultK :: Int
defaultK = 7

-- | FIXME: doc
defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ defaultK `divMod` 2

-- | FIXME: doc
defaultConfig :: KademliaConfig i
defaultConfig
  = KademliaConfig
    { configK               = defaultK
    , configExpirationTime  = hour 1
    , configStoreValueTime  = hour 1
    , configPingTime        = minute 5
    , configNumLookupNodes  = 3
    , configMsgSizeLimit    = 1200
    , configRoutingSharingN = defaultRoutingSharingN
    , configCacheSize       = 5
    , configPingLimit       = 4
    , configSignatureScheme = trivialSignatureScheme
    }

--------------------------------------------------------------------------------

-- |
-- A monad transformer that gives access to a 'KademliaConfig'.
newtype WithConfigT i m a
  = WithConfigT
    { getWithConfigT :: ReaderT (KademliaConfig i) m a
    }
  deriving (Functor, Applicative, Monad, MonadTrans)

--------------------------------------------------------------------------------

-- | FIXME: doc
type WithConfig i a = WithConfigT i Identity a

--------------------------------------------------------------------------------

-- | FIXME: doc
getConfig :: Monad m => WithConfigT i m (KademliaConfig i)
getConfig = WithConfigT ask

-- | FIXME: doc
usingConfigT :: WithConfigT i m a -> KademliaConfig i -> m a
usingConfigT f cfg = flip runReaderT cfg $ getWithConfigT f

-- | FIXME: doc
usingConfig :: WithConfig i a -> KademliaConfig i -> a
usingConfig f cfg = runIdentity $ usingConfigT f cfg

--------------------------------------------------------------------------------

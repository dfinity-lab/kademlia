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
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Control.Monad.Trans         (MonadTrans)
import           DFINITY.Discovery.Signature
                 (SignatureScheme, trivialSignatureScheme)
import           DFINITY.Discovery.Utils     (hour, minute)

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
    , configSignatureScheme :: !(SignatureScheme IO)
      -- ^ FIXME: doc
    }

--------------------------------------------------------------------------------

-- |
-- The default value of the @k@ parameter.
defaultK :: Int
defaultK = 7

-- |
-- The default value of the routing-sharing parameter, which mixes in random
-- nodes in addition to the @k@-closest nodes.
defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ defaultK `divMod` 2

-- |
-- The default 'KademliaConfig' to use.
defaultConfig :: KademliaConfig
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
newtype WithConfigT m a
  = WithConfigT
    { getWithConfigT :: ReaderT KademliaConfig m a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

--------------------------------------------------------------------------------

-- |
-- A useful type alias for 'WithConfigT' when the base monad is 'Identity'.
type WithConfig a = WithConfigT Identity a

--------------------------------------------------------------------------------

-- |
-- Use the monadic context given by the 'WithConfigT' monad transformer to
-- retrieve the configuration ('KademliaConfig').
getConfig :: Monad m => WithConfigT m KademliaConfig
getConfig = WithConfigT ask

-- |
-- Run a 'WithConfigT' computation using the given 'KademliaConfig'.
usingConfigT :: WithConfigT m a -> KademliaConfig -> m a
usingConfigT f cfg = flip runReaderT cfg $ getWithConfigT f

-- |
-- A specialization of 'usingConfigT' where the underlying monad is 'Identity'.
usingConfig :: WithConfig a -> KademliaConfig -> a
usingConfig f cfg = runIdentity $ usingConfigT f cfg

--------------------------------------------------------------------------------

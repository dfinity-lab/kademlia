--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Signature
-- Description: An abstraction representing cryptographic signatures.
--
-- FIXME: doc

--------------------------------------------------------------------------------

module DFINITY.Discovery.Signature
  ( Signature (..)
  , SignatureScheme (..)
  , trivialSignatureScheme
  ) where

--------------------------------------------------------------------------------

import           Codec.Serialise         (Serialise)
import           Data.ByteString         (ByteString)
import           GHC.Generics            (Generic)

import           DFINITY.Discovery.Types (Ident)

--------------------------------------------------------------------------------

newtype Signature
  = Signature { fromSignature :: ByteString }
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialise Signature

--------------------------------------------------------------------------------

data SignatureScheme m
  = SignatureScheme
    { sign   :: ByteString -> m Signature
    , verify :: Ident -> Signature -> ByteString -> m Bool
    }

--------------------------------------------------------------------------------

-- |
-- The trivial 'SignatureScheme', in which a signature is simply an empty string
-- and verification always returns 'True'.
trivialSignatureScheme :: (Applicative m) => SignatureScheme m
trivialSignatureScheme
  = SignatureScheme
    { sign   = \_ -> pure (Signature mempty)
    , verify = \_ _ _ -> pure True
    }

--------------------------------------------------------------------------------

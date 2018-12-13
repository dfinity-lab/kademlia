--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax                #-}

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

import           Data.ByteString (ByteString)

--------------------------------------------------------------------------------

newtype Signature
  = Signature { fromSignature :: ByteString }

--------------------------------------------------------------------------------

data SignatureScheme m pub
  = SignatureScheme
    { sign   :: ByteString -> m Signature
    , verify :: pub -> Signature -> ByteString -> m Bool
    }

--------------------------------------------------------------------------------

-- |
-- The trivial 'SignatureScheme', in which a signature is simply an empty string
-- and verification always returns 'True'.
trivialSignatureScheme :: (Applicative m) => SignatureScheme m any
trivialSignatureScheme
  = SignatureScheme
    { sign   = \_ -> pure (Signature mempty)
    , verify = \_ _ _ -> pure True
    }

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Protocol.Serialize
-- Description: Implementation of the protocol serialization
--
-- "DFINITY.Discovery.Protocol.Serialize" implements the actual serialization of
-- 'Command's.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Protocol.Serialize
  ( serialize
  ) where

--------------------------------------------------------------------------------

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as LBS

import qualified Codec.CBOR.Write        as CBOR
import           Codec.Serialise

import           DFINITY.Discovery.Types (Command, Ident)

--------------------------------------------------------------------------------

serialize
  :: Int
  -> Ident
  -> Command
  -> ByteString
serialize _lim ident command
  = LBS.toStrict
    $ Builder.toLazyByteString
    $ CBOR.toBuilder
    $ encode (ident, command)
-- FIXME: use the lim parameter

--------------------------------------------------------------------------------

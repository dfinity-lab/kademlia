--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Protocol.Parse
-- Description: Implementation of the protocol parsing
--
-- "DFINITY.Discovery.Protocol.Parsing" implements the actual parsing
-- of 'Command's.
--
-- It made sense to split it off of "DFINITY.Discovery.Protocol" as it made both
-- cleaner and more readable.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Protocol.Parse
  ( parse
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow            ((>>>))
import           Data.Function            ((&))

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as LBS

import qualified Codec.CBOR.Read          as CBOR
import           Codec.Serialise
import           Codec.Serialise.Decoding

import           DFINITY.Discovery.Types  (Node (Node), Peer, Signal (Signal))

--------------------------------------------------------------------------------

parse
  :: ByteString
  -> Either String (Peer -> Signal)
parse bs = do
  result <- CBOR.deserialiseFromBytes decodeSignal (LBS.fromStrict bs)
            & either (show >>> Left) Right
  case result of
    ("", x) -> Right x
    (_,  _) -> Left "leftover data"

--------------------------------------------------------------------------------

decodeSignal
  :: Decoder s (Peer -> Signal)
decodeSignal = do
  (ident, command) <- decode
  pure (\peer -> Signal (Node peer ident) command)

--------------------------------------------------------------------------------

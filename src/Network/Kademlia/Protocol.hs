--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Network.Kademlia.Protocol
-- Description : Implementation of the actual protocol
--
-- Network.Kademlia.Protocol implements the parsing and serialisation of
-- ByteStrings into 'Protocol'-Values.

--------------------------------------------------------------------------------

module Network.Kademlia.Protocol
  ( serialize
  , parse
  ) where

--------------------------------------------------------------------------------

import           Control.Monad                     (when)
import           Control.Monad.Except              (throwError)
import           Data.List                         (scanl')
import           Data.Word                         (Word8)

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (toLazyByteString, word16BE)
import qualified Data.ByteString.Lazy              as LBS

import qualified Data.Text.Encoding                as Text

import           Network.Kademlia.Protocol.Parsing (parse)
import           Network.Kademlia.Types
                 (Command (..), Node (..), Serialize (..), peerHost, peerPort)

--------------------------------------------------------------------------------

-- |
-- Retrieve the assigned protocol ID.
commandId :: Command i a -> Word8
commandId PING                 = 0
commandId PONG                 = 1
commandId (STORE _ _)          = 2
commandId (FIND_NODE _)        = 3
commandId (RETURN_NODES _ _ _) = 4
commandId (FIND_VALUE _)       = 5
commandId (RETURN_VALUE _ _)   = 6

--------------------------------------------------------------------------------

-- FIXME: move this into Network.Kademlia.Protocol.Serialize

-- |
-- Turn the command arguments into a ByteString, which fits into specified size.
-- The remaining part of command is also returned, if any.
serialize
  :: (Serialize i, Serialize a)
  => Int
  -> i
  -> Command i a
  -> Either String [ByteString] -- FIXME: replace String with SerializeError
serialize size (toBS -> myId) cmd = do
  case cmd of
    (RETURN_NODES _ (toBS -> nid) allNodes) -> do
      let prefix :: Word8 -> ByteString
          prefix n = commandId cmd `BS.cons` myId `BS.append` (n `BS.cons` nid)
      let addPrefix packs
            = map (prefix (fromIntegral (length packs)) `BS.append`) packs
      let prefixSize = size - 2 - BS.length myId - BS.length nid
      let genPacks :: (Serialize i) => [Node i] -> Either String [ByteString]
          genPacks [] = pure []
          genPacks nodes = do
            let fits b     = BS.length b < prefixSize
            let incPacks   = takeWhile fits
                             $ scanl' BS.append BS.empty
                             $ map nodeToArg nodes
            let argsFitNum = length incPacks
            let pack       = last incPacks
            when (argsFitNum == 0) $ do
              throwError "No nodes fit on RETURN_NODES serialization"
            (pack :) <$> genPacks (drop argsFitNum nodes)
      addPrefix <$> genPacks allNodes
    _ -> do
      let res = case cmd of
                  PING                 -> BS.empty
                  PONG                 -> BS.empty
                  (FIND_NODE nid)      -> toBS nid
                  (RETURN_NODES _ _ _) -> error "Don't know what to do with this case :("
      if BS.length res + 1 + BS.length myId > size
        then throwError "Size exceeded"
        else pure [commandId cmd `BS.cons` myId `BS.append` res]

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
nodeToArg :: (Serialize i) => Node i -> ByteString
nodeToArg node
  = let nid = toBS (nodeId node)
        host = peerHost (nodePeer node)
        port = toBinary (fromIntegral (peerPort (nodePeer node)))
        -- Converts a Word16 into a two character ByteString
        toBinary = BS.concat . LBS.toChunks . toLazyByteString . word16BE
    in nid `BS.append` Text.encodeUtf8 (host <> " ") `BS.append` port

--------------------------------------------------------------------------------

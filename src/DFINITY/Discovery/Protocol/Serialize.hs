--------------------------------------------------------------------------------

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as LBS

import qualified Codec.CBOR.Write        as CBOR
import           Codec.Serialise         (Serialise, encode)

import           DFINITY.Discovery.Types
                 (Command (RETURN_NODES), Ident (Ident, fromIdent), Node)

--------------------------------------------------------------------------------

serialize
  :: Int
  -> Ident
  -> Command
  -> [ByteString]
serialize lim ident command = map (encodeToBS . (ident,)) (splitUp lim command)

encodeToBS :: (Serialise a) => a -> ByteString
encodeToBS = LBS.toStrict
             . Builder.toLazyByteString
             . CBOR.toBuilder
             . encode

splitUp :: Int -> Command -> [Command]
splitUp lim (RETURN_NODES _ ident nodes)
  = let xs = splitUpReturnNodes lim (ident, nodes)
        l = length xs
        i = if | (l > 255) -> error "splitUp: length outside Word8 upper bound"
               | (l < 0)   -> error "splitUp: length outside Word8 lower bound"
               | otherwise -> fromIntegral l
    in map (\(n, ns) -> RETURN_NODES i n ns) xs
splitUp _ command = [command]

splitUpReturnNodes
  :: Int
  -> (Ident, [Node])
  -> [(Ident, [Node])]
splitUpReturnNodes lim (ident, nodes)
  = if | (limit <= 0) -> error "splitUpReturnNodes: limit is negative"
       | otherwise    -> go 0 [] nodes
  where
    limit :: Int
    limit = lim
            - (pairOverhead
               + identOverhead + BS.length (fromIdent ident)
               + listOverhead)
            - 12 -- empirically determined extra buffer space

    nodeSize :: Node -> Int
    nodeSize = BS.length . encodeToBS
    -- FIXME: ^ is a bit slow, but since we may change Node soon it's not worth
    --        speeding up for now.

    go :: Int -> [Node] -> [Node] -> [(Ident, [Node])]
    go _    []    []     = []
    go _    soFar []     = [(ident, reverse soFar)]
    go size soFar (n:ns) = let size' = size + nodeSize n
                           in if size' >= limit
                              then (ident, reverse soFar) : go 0 [] (n : ns)
                              else go size' (n : soFar) ns

identOverhead :: Int
identOverhead
  = BS.length (encodeToBS (Ident "foobar")) - 6

pairOverhead :: Int
pairOverhead
  = BS.length (encodeToBS (("foobar", "bazqux") :: (ByteString, ByteString)))
    - (2 * (6 + bsOverhead))

listOverhead :: Int
listOverhead
  = BS.length (encodeToBS (["foobar", "bazqux", "gargle"] :: [ByteString]))
    - (3 * (6 + bsOverhead))

bsOverhead :: Int
bsOverhead
  = BS.length (encodeToBS ("foobar" :: ByteString)) - BS.length "foobar"

--------------------------------------------------------------------------------

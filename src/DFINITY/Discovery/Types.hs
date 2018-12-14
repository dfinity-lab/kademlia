--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Types
-- Description: Definitions of a few types
--
-- "DFINITY.Discovery.Types" defines a few types that are used throughout the
-- @dfinity-discovery@ codebase.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Types
  ( ByteStruct
  , Command (..)
  , Node    (..)
  , Peer    (..)
  , Signal  (..)
  , Ident   (..)
  , Value   (..)
  , Timestamp

  , distance
  , fromByteStruct
  , sortByDistanceTo
  , toByteStruct
  , toPeer
  , unwrapPort
  , wrapPort
  ) where

--------------------------------------------------------------------------------

import           Data.Bits                (setBit, testBit, zeroBits)
import           Data.Function            (on)
import           Data.Int                 (Int64)
import           Data.List                (sortBy)
import           Data.Word                (Word16, Word8)
import           GHC.Generics             (Generic)
import           Network.Socket           (PortNumber (..), SockAddr (..))

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS

import           Data.IP                  (IP)
import qualified Data.IP                  as IP

import           Codec.Serialise          (Serialise, decode, encode)
import           Codec.Serialise.Decoding (decodeListLen, decodeWord)
import           Codec.Serialise.Encoding (encodeListLen, encodeWord)

--------------------------------------------------------------------------------

newtype Ident
  = Ident { fromIdent :: ByteString }
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialise Ident

--------------------------------------------------------------------------------

newtype Value
  = Value { fromValue :: ByteString }
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialise Value

--------------------------------------------------------------------------------

-- | Representation of a UDP peer.
data Peer
  = Peer
    { peerHost :: !IP
    , peerPort :: !PortNumber
    }
  deriving (Eq, Ord, Generic)

instance Show Peer where
  show (Peer h p) = show h ++ ":" ++ show p

instance Serialise Peer where
  encode (Peer (IP.IPv4 ip) port)
    = encodeListLen 3
      <> encodeWord 0
      <> encode (IP.toHostAddress ip)
      <> encode (fromEnum port)
  encode (Peer (IP.IPv6 ip) port)
    = encodeListLen 3
      <> encodeWord 1
      <> encode (IP.toHostAddress6 ip)
      <> encode (fromEnum port)
  decode = do
    len <- decodeListLen
    tag <- decodeWord
    ip <- case (len, tag) of
      (3, 0) -> IP.IPv4 . IP.fromHostAddress  <$> decode
      (3, 1) -> IP.IPv6 . IP.fromHostAddress6 <$> decode
      _      -> fail "invalid Peer encoding"
    port <- toEnum <$> decode
    pure $ Peer ip port

unwrapPort :: PortNumber -> Word16
unwrapPort = fromIntegral

wrapPort :: Word16 -> PortNumber
wrapPort = fromIntegral

--------------------------------------------------------------------------------

-- | Representation of a Kademlia 'Node', containing a 'Peer' and an ID.
data Node
  = Node
    { nodePeer :: !Peer
      -- FIXME: switch to MaxHeap Timestamp Peer
      --        where `type MaxHeap p a = Heap (Entry (Down p) a)`
      --        is using the `heaps` package
    , nodeId   :: !Ident
    }
  deriving (Eq, Ord, Generic)

instance Serialise Node

instance Show Node where
  show (Node peer ident) = show peer ++ " (" ++ show ident ++ ")"

-- createNode
--   :: (MonadIO m)
--   => Peer
--   -> Ident
--   -> WithConfigT m Node
-- createNode peer ident = do
--   sigScheme <- configSignatureScheme <$> getConfig
--   sig <- liftIO $ sign sigScheme (LBS.toStrict (serialise peer))
--   pure (Node peer ident sig)
--
-- verifyNode
--   :: (MonadIO m)
--   => Node
--   -> WithConfigT m Bool
-- verifyNode (Node peer ident sig) = do
--   sigScheme <- configSignatureScheme <$> getConfig
--   liftIO $ verify sigScheme ident sig (LBS.toStrict (serialise peer))

--------------------------------------------------------------------------------

-- | Sort a bucket by the closeness of its nodes to a given ID.
sortByDistanceTo
  :: [Node]
  -> Ident
  -> [Node]
sortByDistanceTo bucket nid = do
  let f = distance nid . nodeId
  let pack bk = zip bk (map f bk)
  let sort = sortBy (compare `on` snd)
  let unpack = map fst
  unpack (sort (pack bucket))

--------------------------------------------------------------------------------

-- | A Structure made up of bits, represented as a list of Bools
--
--   FIXME: this is really bad
type ByteStruct = [Bool]

--------------------------------------------------------------------------------

-- | Converts a Serialize into a ByteStruct
toByteStruct
  :: Ident
  -> ByteStruct
toByteStruct
  = BS.foldr (\w bits -> convert w ++ bits) [] . fromIdent
  where
    convert w = foldr (\i bits -> testBit w i : bits) [] [0 .. 7]

--------------------------------------------------------------------------------

-- | Convert a ByteStruct back to its ByteString form
fromByteStruct
  :: ByteStruct
  -> Ident
fromByteStruct bs
  = Ident $ BS.pack $ foldr (\i ws -> createWord i : ws) [] indexes
  where
    indexes = [0 .. (length bs `div` 8) - 1]
    createWord i = let pos = i * 8
                   in foldr changeBit zeroBits [pos .. pos + 7]
    changeBit i w = if bs !! i
                    then setBit w (i `mod` 8)
                    else w

--------------------------------------------------------------------------------

-- |
-- Calculate the distance between two IDs using the XOR metric as specified in
-- the Kademlia paper.
distance
  :: Ident
  -> Ident
  -> ByteStruct
distance idA idB
  = let xor a b = not (a && b) && (a || b)
        bsA = toByteStruct idA
        bsB = toByteStruct idB
    in zipWith xor bsA bsB

--------------------------------------------------------------------------------

-- | Try to convert a 'SockAddr' to a 'Peer'
toPeer :: SockAddr -> IO (Maybe Peer)
toPeer sockAddr = pure (uncurry Peer <$> IP.fromSockAddr sockAddr)

--------------------------------------------------------------------------------

-- |
-- Representation of a protocol signal.
data Signal
  = Signal
    { signalSource  :: !Node
    , signalCommand :: !Command
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- |
-- Representation of the different protocol commands.
data Command
  = PING
  | PONG
  | STORE        !Ident !Value
  | FIND_NODE    !Ident
  | RETURN_NODES !Word8 !Ident ![Node]
  | FIND_VALUE   !Ident
  | RETURN_VALUE !Ident !Value
  deriving (Eq)

instance Serialise Command where
  encode PING                         = encodeListLen 1 <> encodeWord 0
  encode PONG                         = encodeListLen 1 <> encodeWord 1
  encode (STORE ident payload)        = encodeListLen 3 <> encodeWord 2
                                        <> encode ident
                                        <> encode payload
  encode (FIND_NODE ident)            = encodeListLen 2 <> encodeWord 3
                                        <> encode ident
  encode (RETURN_NODES n ident nodes) = encodeListLen 4 <> encodeWord 4
                                        <> encode n
                                        <> encode ident
                                        <> encode nodes
  encode (FIND_VALUE ident)           = encodeListLen 2 <> encodeWord 5
                                        <> encode ident
  encode (RETURN_VALUE ident payload) = encodeListLen 3 <> encodeWord 6
                                        <> encode ident
                                        <> encode payload

  decode = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
      (1, 0) -> pure PING
      (1, 1) -> pure PONG
      (3, 2) -> STORE <$> decode <*> decode
      (2, 3) -> FIND_NODE <$> decode
      (4, 4) -> RETURN_NODES <$> decode <*> decode <*> decode
      (2, 5) -> FIND_VALUE <$> decode
      (3, 6) -> RETURN_VALUE <$> decode <*> decode
      _      -> fail "invalid Command encoding"

instance Show Command where
  show PING                     = "PING"
  show PONG                     = "PONG"
  show (STORE i _)              = "STORE " ++ show i ++ " <data>"
                                  -- FIXME: encode bytestring as hex here
  show (FIND_VALUE i)           = "FIND_VALUE " ++ show i
  show (RETURN_VALUE i _)       = "RETURN_VALUE " ++ show i ++ " <data>"
                                  -- FIXME: encode bytestring as hex here
  show (FIND_NODE i)            = "FIND_NODE " ++ show i
  show (RETURN_NODES n i nodes) = "RETURN_NODES "
                                  ++ "(one of " ++ show n ++ " messages) "
                                  ++ show i ++ " " ++ show nodes

--------------------------------------------------------------------------------

-- |
-- Number of seconds since 1970-01-01 00:00 UTC, ignoring leap seconds.
type Timestamp = Int64

--------------------------------------------------------------------------------

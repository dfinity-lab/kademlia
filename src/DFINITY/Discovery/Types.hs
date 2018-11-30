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
  , Command   (..)
  , Node      (..)
  , Peer      (..)
  , Serialize (..)
  , Signal    (..)
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
import           Network.Socket
                 (PortNumber (..), SockAddr (..), inet_ntoa)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS

import           Data.Text                (Text)
import qualified Data.Text                as Text

import           DFINITY.Discovery.Config (WithConfig, getConfig)
import qualified DFINITY.Discovery.Config as C

--------------------------------------------------------------------------------

-- | Representation of a UDP peer.
data Peer
  = Peer
    { peerHost :: !Text
    , peerPort :: !PortNumber
    }
  deriving (Eq, Ord, Generic)

instance Show Peer where
  show (Peer h p) = Text.unpack h ++ ":" ++ show p

unwrapPort :: PortNumber -> Word16
unwrapPort = fromIntegral

wrapPort :: Word16 -> PortNumber
wrapPort = fromIntegral

--------------------------------------------------------------------------------

-- | Representation of a Kademlia 'Node', containing a 'Peer' and an ID.
data Node i
  = Node
    { nodePeer :: !Peer
    , nodeId   :: !i
    }
  deriving (Eq, Ord, Generic)

instance Show i => Show (Node i) where
  show (Node peer ident) = show peer ++ " (" ++ show ident ++ ")"

--------------------------------------------------------------------------------

-- | Sort a bucket by the closeness of its nodes to a given ID.
sortByDistanceTo
  :: (Serialize i)
  => [Node i]
  -> i
  -> WithConfig [Node i]
sortByDistanceTo bucket nid = do
  let f = distance nid . nodeId
  let pack bk = zip bk <$> mapM f bk
  let sort = sortBy (compare `on` snd)
  let unpack = map fst
  unpack . sort <$> pack bucket

--------------------------------------------------------------------------------

-- | A structure serializable into and parsable from a 'ByteString'.
class Serialize a where
  fromBS :: ByteString -> Either String (a, ByteString)
  toBS   :: a -> ByteString

--------------------------------------------------------------------------------

-- | A Structure made up of bits, represented as a list of Bools
type ByteStruct = [Bool]

--------------------------------------------------------------------------------

-- | Converts a Serialize into a ByteStruct
toByteStruct
  :: (Serialize a)
  => a
  -> WithConfig ByteStruct
toByteStruct s = do
    k <- C.configK <$> getConfig
    let convert w = map (testBit w) [0..k]
    return $ BS.foldr (\w bits -> convert w ++ bits) [] $ toBS s

--------------------------------------------------------------------------------

-- | Convert a ByteStruct back to its ByteString form
fromByteStruct
  :: (Serialize a)
  => ByteStruct
  -> WithConfig a
fromByteStruct bs = do
    k <- C.configK <$> getConfig

    let changeBit i w = if bs !! i then setBit w (i `mod` (k + 1)) else w
    let createWord i = let pos = i * (k + 1)
                       in foldr changeBit zeroBits [pos .. pos + k]
    let indexes = [0 .. (length bs `div` (k + 1)) - 1]
    let s = BS.pack (map createWord indexes)

    case fromBS s of
        (Right (converted, _)) -> return converted
        (Left err)             -> error $ "Failed to convert from ByteStruct: " ++ err

--------------------------------------------------------------------------------

-- |
-- Calculate the distance between two IDs using the XOR metric as specified in
-- the Kademlia paper.
distance
  :: (Serialize i)
  => i
  -> i
  -> WithConfig ByteStruct
distance idA idB = do
  let xor a b = not (a && b) && (a || b)
  bsA <- toByteStruct idA
  bsB <- toByteStruct idB
  pure $ zipWith xor bsA bsB

--------------------------------------------------------------------------------

-- | Try to convert a 'SockAddr' to a 'Peer'
toPeer :: SockAddr -> IO (Maybe Peer)
toPeer (SockAddrInet port host) = do hostname <- Text.pack <$> inet_ntoa host
                                     pure $ Just (Peer hostname port)
toPeer _                        = pure Nothing

--------------------------------------------------------------------------------

-- |
-- Representation of a protocol signal.
data Signal i v
  = Signal
    { signalSource  :: !(Node i)
    , signalCommand :: !(Command i v)
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- |
-- Representation of the different protocol commands.
data Command i a
  = PING
  | PONG
  | STORE        !i !a
  | FIND_NODE    !i
  | RETURN_NODES !Word8 !i ![Node i]
  | FIND_VALUE   !i
  | RETURN_VALUE !i !a
  deriving (Eq)

instance Show i => Show (Command i a) where
  show PING                     = "PING"
  show PONG                     = "PONG"
  show (STORE i _)              = "STORE " ++ show i ++ " <data>"
  show (FIND_VALUE i)           = "FIND_VALUE " ++ show i
  show (RETURN_VALUE i _)       = "RETURN_VALUE " ++ show i ++ " <data>"
  show (FIND_NODE i)            = "FIND_NODE " ++ show i
  show (RETURN_NODES n i nodes) = "RETURN_NODES "
                                  ++ "(one of " ++ show n ++ " messages) "
                                  ++ show i ++ " " ++ show nodes

--------------------------------------------------------------------------------

-- |
-- Number of seconds since 1970-01-01 00:00 UTC, ignoring leap seconds.
type Timestamp = Int64

--------------------------------------------------------------------------------

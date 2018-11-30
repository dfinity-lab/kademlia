--------------------------------------------------------------------------------

-- |
-- Module      : Network.Kademlia.Protocol.Parse
-- Description : Implementation of the protocol parsing
--
-- "Network.Kademlia.Protocol.Parsing" implements the actual parsing
-- of 'Command's.
--
-- It made sense to split it off of "Network.Kademlia.Protocol" as it made both
-- cleaner and more readable.

--------------------------------------------------------------------------------

module Network.Kademlia.Protocol.Parse
  ( parse
  , parseSerialize
  , parseSignal
  , skipCharacter
  , parseWord16
  , parseWord8
  ) where

--------------------------------------------------------------------------------

import           Control.Monad.State        (State, evalState, get, put)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
                 (ExceptT, catchE, runExceptT, throwE)
import           Data.Bits                  (shiftL)
import           Data.Word                  (Word16, Word8)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC8

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text

import           Network.Kademlia.Types
                 (Command (FIND_NODE, PING, PONG, RETURN_NODES), Node (Node),
                 Peer (Peer), Serialize (fromBS), Signal (Signal))

--------------------------------------------------------------------------------

-- FIXME: switch to a wire protocol based on CBOR using `serialise`

--------------------------------------------------------------------------------

data ParseError
  = ParseError_UnexpectedEndOfInput
  | ParseError_InvalidCommandID
  | ParseError_SerializeFailed !Text
  deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

type Parse = ExceptT ParseError (State ByteString)

--------------------------------------------------------------------------------

-- |
-- Parse a signal from a ByteString
--
-- This needs to be supplied a 'Peer' so it can create a complete 'Signal'.
parse
  :: (Serialize i, Serialize a)
  => Peer
  -> ByteString
  -> Either ParseError (Signal i a)
parse peer = evalState (runExceptT (parseSignal peer))

--------------------------------------------------------------------------------

-- | Parses the parsable parts of a signal
parseSignal
  :: (Serialize i, Serialize a)
  => Peer
  -> Parse (Signal i a)
parseSignal peer = do
  cId <- parseWord8
  nid <- parseSerialize
  cmd <- parseCommand cId
  let node = Node peer nid
  pure $ Signal node cmd

--------------------------------------------------------------------------------

-- | Parses a type with a 'Serialize' instance.
parseSerialize
  :: (Serialize a)
  => Parse a
parseSerialize = do
  bs <- lift get
  case fromBS bs of
    Left          err -> throwE (ParseError_SerializeFailed (Text.pack err))
    Right (nid, rest) -> lift (put rest) >> pure nid

--------------------------------------------------------------------------------

-- | Splits after a certain character
parseSplit
  :: Char
  -> Parse ByteString
parseSplit c = do
  bs <- lift get
  if BS.null bs
    then throwE ParseError_UnexpectedEndOfInput
    else do let (result, rest) = BSC8.span (/= c) bs
            lift (put rest)
            return result

--------------------------------------------------------------------------------

-- | Skips one character
skipCharacter
  :: Parse ()
skipCharacter = do
  bs <- lift get
  if BS.null bs
    then throwE ParseError_UnexpectedEndOfInput
    else lift (put (BS.drop 1 bs))

--------------------------------------------------------------------------------

-- | Parses a 'Word8'.
parseWord8
  :: Parse Word8
parseWord8 = do
  bs <- lift get
  if BS.null bs
    then throwE ParseError_UnexpectedEndOfInput
    else do lift (put (BS.tail bs))
            pure (BS.head bs)

--------------------------------------------------------------------------------

-- | Parses two 'Word8's from a 'ByteString' into one 'Word16'.
parseWord16
  :: Parse Word16
parseWord16 = do
  let toWord16 :: Word8 -> Word16
      toWord16 = fromIntegral
  let joinWords [a, b] = (toWord16 a `shiftL` 8) + toWord16 b
      joinWords _      = error "Panic! Error in join words"
  bs <- lift get
  if BS.length bs < 2
    then throwE ParseError_UnexpectedEndOfInput
    else do let (byteWords, rest) = BS.splitAt 2 bs
            lift (put rest)
            pure (joinWords (BS.unpack byteWords))

--------------------------------------------------------------------------------

-- | Parses a 'Node'.
parseNode
  :: (Serialize i)
  => Parse (Node i)
parseNode = do
  nid <- parseSerialize
  host <- parseSplit ' '
  skipCharacter
  port <- parseWord16
  let peer = Peer (Text.decodeUtf8 host) (fromIntegral port)
  pure $ Node peer nid

--------------------------------------------------------------------------------

-- | Parses a trailing k-bucket.
parseKBucket
  :: (Serialize i)
  => Parse [Node i]
parseKBucket
  = ((:) <$> parseNode <*> parseKBucket)
    `catchE` (\_ -> pure [])

--------------------------------------------------------------------------------

-- | Parses the rest of a command using the given prefix ID number.
parseCommand
  :: (Serialize i, Serialize a)
  => Word8
  -> Parse (Command i a)
parseCommand 0 = pure PING
parseCommand 1 = pure PONG
parseCommand 2 = STORE <$> parseSerialize <*> parseSerialize
parseCommand 3 = FIND_NODE <$> parseSerialize
parseCommand 4 = RETURN_NODES <$> parseWord8 <*> parseSerialize <*> parseKBucket
parseCommand 5 = FIND_VALUE <$> parseSerialize
parseCommand 6 = RETURN_VALUE <$> parseSerialize <*> parseSerialize
parseCommand _ = throwE ParseError_InvalidCommandID

--------------------------------------------------------------------------------

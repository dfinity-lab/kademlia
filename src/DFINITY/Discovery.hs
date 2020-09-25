--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery
-- Description: Implementation of the Kademlia DHT
-- License:     BSD3
-- Maintainer:  team@dfinity.org
-- Stability:   experimental
-- Portability: GHC
--
-- A Haskell implementation of the Kademlia distributed hashtable, an efficient
-- way to store and lookup values distributed over a P2P network.
--
-- The implementation is based on the paper by Petar Maymounkov and David Mazières:
-- /Kademlia: A Peer-to-peer Information System Based on the XOR Metric/:
-- <http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>
--
-- This library aims to be very simple and pleasant to use, with the downside of
-- deciding some of the implementation details, like timeout intervals and
-- k-bucket size, for the user.
--
-- = How to use it
--
-- To get started with this library, first import it.
--
-- > import DFINITY.Discovery (close, create)
-- > import DFINITY.Discovery.Types (Ident (..), Peer (..), Value (..))
-- > import qualified DFINITY.Discovery.Implementation as I
-- Next, you need to decide on the types you want to use as the values to be stored
-- in the DHT and the keys to acces them by. As soon as you've decided on them, you
-- have to make them instances of the "Serialize" typeclass, so they can be sent over
-- the network.
--
-- > import qualified Data.ByteString.Char8 as C
--
--
-- Now you're ready to dive in and use the DHT:
--
-- > main = do
-- >    let idA = Ident (C.pack $ "good guy")
-- >    let idB = Ident (C.pack $ "bad guy")
-- >    let idX = Ident (C.pack $ "idx")
-- >    let valX = Value (C.pack $ "hello world")
-- >    -- Create the first instance, which will serve as the first node of the
-- >    -- network
-- >    firstInstance <- create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idA
-- >
-- >    -- Create a Node representing the first instance
-- >    let firstNode = Peer "127.0.0.1" 1123
-- >
-- >    -- Create the second instance and make it join the network
-- >    secondInstance <- create ("127.0.0.1", 1124) ("127.0.0.1", 1124) idB
-- >    joinResult <- I.joinNetwork secondInstance firstNode
-- >
-- >    -- Make sure the joining was successful
-- >    case joinResult of
-- >         I.JoinSuccess -> do
-- >             -- Store an example value in the network
-- >             I.store secondInstance idX valX
-- >             -- Look up the value and it's source
-- >             res <- I.lookup firstInstance idX
-- >             case res of
-- >               Just (value, _) -> print $ (show idX) ++ " " ++ (show value)
-- >               Nothing -> print $ "Not Found Key" ++ (show idX)
-- >
-- >         _ -> return ()
-- >
-- >    -- Close the instances
-- >    close firstInstance
-- >    close secondInstance
--
-- As promised, the usage of the actual DHT is rather easy. There are a few things
-- to note, though:
--
--     * To join an existing network, you need to know the hostname, listening port
--       and id of a node that is already part of that network
--     * When you don't need access to the DHT anymore, make sure to close the instances.
--       This closes opened sockets and kills the threads running in the background
--
-- Another thing to note is, that you are responsible for assigning ids to nodes
-- and keys to values, as well as making sure these are unique. The Kademlia paper
-- doesn't propose any measures for this and, as this library is just a
-- implementation of the system proposed in it, this library doesn't implement
-- anything to handle this.
--
-- FIXME: update these docs to account for the fact that we can no longer
--        store or lookup items in the tree; we only have discovery now.

--------------------------------------------------------------------------------

module DFINITY.Discovery
  ( KademliaInstance
  , instanceNode
  , KademliaConfig (..)
  , KademliaSnapshot (..)
  , create
  , createL
  , createLFromSnapshot
  , defaultConfig
  , close
  , I.lookup
  , I.store
  , I.lookupNode
  , I.joinNetwork
  , viewBuckets
  , peersToNodeIds
  , dumpPeers
  , banNode
  , isNodeBanned
  , takeSnapshot
  , restoreInstance
  , distance
  , sortByDistanceTo
  , usingKademliaInstance
  , JoinResult (..)
  , Node (..)
  , Peer (..)
  ) where

--------------------------------------------------------------------------------

import           Data.IP                          (IP)
import           Network.Socket                   (PortNumber)

import           DFINITY.Discovery.Config
import           DFINITY.Discovery.Implementation as I
import           DFINITY.Discovery.Instance
import           DFINITY.Discovery.Networking
import           DFINITY.Discovery.Process        (start)
import           DFINITY.Discovery.ReplyQueue
import qualified DFINITY.Discovery.Tree           as T
import           DFINITY.Discovery.Types

--------------------------------------------------------------------------------

-- |
-- Create a new 'KademliaInstance' corresponding to a given ID on a given port.
create
  :: (IP, PortNumber)
  -- ^ Bind address
  -> (IP, PortNumber)
  -- ^ External address
  -> Ident
  -> IO KademliaInstance
create bindAddr extAddr id' = do
  createL bindAddr extAddr id' defaultConfig (const (pure ())) (const (pure ()))

--------------------------------------------------------------------------------

-- |
-- Same as 'create', but with logging
createL
  :: (IP, PortNumber)
  -- ^ Bind address
  -> (IP, PortNumber)
  -- ^ External address
  -> Ident
  -> KademliaConfig
  -> (String -> IO ())
  -> (String -> IO ())
  -> IO KademliaInstance
createL (host, port) extAddr id' cfg logInfo logError = do
  rq <- emptyReplyQueueL logInfo logError
  let lim = configMsgSizeLimit cfg
  h <- openOnL host port id' lim rq logInfo logError
  inst <- newInstance id' extAddr cfg h
  start inst
  pure inst

--------------------------------------------------------------------------------

-- | Create instance with logging from the given 'KademliaSnapshot'.
createLFromSnapshot
  :: (IP, PortNumber)
  -- ^ Bind address
  -> (IP, PortNumber)
  -- ^ External address
  -> KademliaConfig
  -> KademliaSnapshot
  -> (String -> IO ())
  -> (String -> IO ())
  -> IO KademliaInstance
createLFromSnapshot (host, port) extAddr cfg snapshot logInfo logError = do
  rq <- emptyReplyQueueL logInfo logError
  let lim = configMsgSizeLimit cfg
  let id' = T.extractId (snapshotTree snapshot)
  h <- openOnL host port id' lim rq logInfo logError
  inst <- restoreInstance extAddr cfg h snapshot
  start inst
  pure inst

--------------------------------------------------------------------------------

-- | Stop a 'KademliaInstance' by 'close'ing it.
close
  :: KademliaInstance
  -> IO ()
close = closeK . instanceHandle

--------------------------------------------------------------------------------

-- | Run 'WithConfig' action using the given 'KademliaInstance'.
usingKademliaInstance
  :: WithConfig a
  -> KademliaInstance
  -> a
usingKademliaInstance f = usingConfig f . instanceConfig

--------------------------------------------------------------------------------

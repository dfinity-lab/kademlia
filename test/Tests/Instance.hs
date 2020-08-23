--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Instance
-- Description: Tests for DFINITY.Discovery.Instance
--
-- Tests specific to "DFINITY.Discovery.Instance".

--------------------------------------------------------------------------------

module Tests.Instance
  ( handlesPingCheck
  , trackingKnownPeersCheck
  , isNodeBannedCheck
  , banNodeCheck
  , snapshotCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Chan      (readChan, writeChan)
import           Control.Monad                (liftM2, void)
import qualified Data.ByteString.Char8        as C
import           Data.Function                (on)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, isJust)

import           Data.List                    (sort)
import           Test.HUnit
                 (Assertion, assertEqual, assertFailure)
import           Test.QuickCheck
                 (Property, arbitrary, conjoin, (===))
import           Test.QuickCheck.Monadic
                 (PropertyM, assert, monadicIO, pick, run)

import           DFINITY.Discovery            (close, create)
import           DFINITY.Discovery.Instance
                 (BanState (..), KademliaInstance (..), KademliaSnapshot (..),
                 banNode, dumpPeers, isNodeBanned, lookupNode)
import           DFINITY.Discovery.Networking
                 (closeK, openOn, send, startRecvProcess)
import           DFINITY.Discovery.ReplyQueue
                 (Reply (..), ReplyQueue (..), emptyReplyQueue)
import qualified DFINITY.Discovery.Tree       as T
import           DFINITY.Discovery.Types
                 (Command (..), Ident (..), Node (..), Peer (..), Signal (..),
                 Timestamp)

import           Tests.TestTypes              (NodeBunch (..))
import           Tests.Tree                   (withTree)

--------------------------------------------------------------------------------

-- | The default set of peers
peers :: (Peer, Peer)
peers = let pA = Peer "127.0.0.1" 1122
            pB = Peer "127.0.0.1" 1123
        in (pA, pB)

-- | A set of randomly generated Ids
ids :: (Monad m) => PropertyM m (Ident, Ident)
ids = liftM2 (,) (pick arbitrary) (pick arbitrary)

-- | Checks whether PINGs are handled appropriately
handlesPingCheck :: Assertion
handlesPingCheck = do
    let (_, pB) = peers

    let idA = Ident (C.replicate 32 'a')
    let idB = Ident (C.replicate 32 'b')

    rq <- emptyReplyQueue

    khA <- openOn "127.0.0.1" 1122 idA rq
    kiB <- create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idB
           :: IO KademliaInstance

    startRecvProcess khA

    send khA pB PING
    (Answer sig) <- readChan (replyQueueDispatchChan rq)

    closeK khA
    close kiB

    assertEqual "" (signalCommand sig) PONG
    assertEqual "" (nodePeer (signalSource sig)) pB
    assertEqual "" (nodeId   (signalSource sig)) idB

    return ()

-- | Assert that a peer is put into the NodeTree on first encounter
trackingKnownPeersCheck :: Property
trackingKnownPeersCheck = monadicIO $ do
    let (_, pB) = peers
    (idA, idB) <- ids

    (node, kiB) <- run $ do
        rq <- emptyReplyQueue :: IO ReplyQueue

        khA <- openOn "127.0.0.1" 1122 idA rq
        kiB <- create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idB

        startRecvProcess khA

        send khA pB PING
        () <$ readChan (replyQueueDispatchChan rq)

        node <- lookupNode kiB idA

        closeK khA
        close kiB

        return (node, kiB)

    assert . isJust $ node

    ns <- run . dumpPeers $ kiB
    assert $ ns == [(fromJust node, 0 :: Timestamp)]

    return ()

-- | Make sure `isNodeBanned` works correctly
isNodeBannedCheck :: Assertion
isNodeBannedCheck = do
    let (idA, idB) = (Ident (C.pack "hello"), Ident (C.pack "salve"))
    let (peerA, peerB) = (Peer "127.0.0.1" 1123, Peer "127.0.0.1" 1124)
    let (nodeA, nodeB) = (Node peerA idA, Node peerB idB)

    inst <- create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idA

    let check msg ans = do
            ban <- isNodeBanned inst peerB
            assertEqual msg ban ans

    check "Initial" False

    banNode inst nodeB BanForever
    check "Plain ban set" True

    banNode inst nodeB NoBan
    check "Reset ban to False" False

    close inst

-- | Messages from banned node are ignored
banNodeCheck :: Assertion
banNodeCheck = do
    let (peerA, peerB) = peers

    let idA = Ident (C.replicate 32 'a')
    let idB = Ident (C.replicate 32 'b')

    rq <- emptyReplyQueue

    khA <- openOn "127.0.0.1" 1122 idA rq
    kiB <- create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idB

    banNode kiB (Node peerA idA) BanForever
    startRecvProcess khA

    send khA peerB PING

    -- if no message received for long enough, put OK message
    void $ forkIO $ do
        threadDelay 10000
        writeChan (replyQueueDispatchChan rq) Closed

    res <- readChan (replyQueueDispatchChan rq)

    closeK khA
    close kiB

    case res of
        Closed -> return ()
        _      -> assertFailure "Message from banned node isn't ignored"

    return ()

-- Snapshot is serialized and deserialized well
snapshotCheck :: NodeBunch -> Ident -> [BanState] -> Property
snapshotCheck = withTree $ \tree ns -> pure $ \bans ->
        let banned = M.fromList $ zip (map nodePeer ns) bans
            sp     = KademliaSnapshot tree banned mempty
            sp'    = sp -- FIXME: decode (encode sp)
        in  conjoin [ ((===) `on` snapshotBanned)                 sp sp'
                    , ((===) `on` sort . T.toList . snapshotTree) sp sp'
                    ]

--------------------------------------------------------------------------------

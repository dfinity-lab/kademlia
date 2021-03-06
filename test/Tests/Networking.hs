--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Networking
-- Description: Tests for DFINITY.Discovery.Networking
--
-- Tests specific to "DFINITY.Discovery.Networking".

--------------------------------------------------------------------------------

module Tests.Networking
  ( expectCheck
  , sendCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.Chan      (Chan, newChan, readChan)
import           Data.Maybe                   (isJust)
import           Test.QuickCheck              (Gen, Property, arbitrary)
import           Test.QuickCheck.Monadic
                 (PropertyM, assert, monadicIO, pick, pre, run)

import           DFINITY.Discovery.Networking
                 (KademliaHandle (..), closeK, expect, openOn, send,
                 startRecvProcess)
import           DFINITY.Discovery.ReplyQueue
                 (Reply (..), ReplyQueue (..),
                 ReplyRegistration (ReplyRegistration), ReplyType (..),
                 dispatch, emptyReplyQueue)
import           DFINITY.Discovery.Types
                 (Command (..), Ident, Node (..), Peer (..), Signal (..))

import           Tests.TestTypes              ()

--------------------------------------------------------------------------------

valueSet :: (Monad m) => PropertyM m (Peer, Peer, Ident, Ident)
valueSet = do
    let pA = Peer "127.0.0.1" 1122
        pB = Peer "127.0.0.1" 1123

    idA <- pick (arbitrary :: Gen Ident)
    idB <- pick (arbitrary :: Gen Ident)

    return (pA, pB, idA, idB)

-- | Make sure sending and receiving works
sendCheck :: Command -> Property
sendCheck cmd = monadicIO $ do
    (pA, pB, idA, idB) <- valueSet

    sig <- run $ do
        rqA <- emptyReplyQueue
        rqB <- emptyReplyQueue

        khA <- openOn "127.0.0.1" 1122 idA rqA
        khB <- openOn "127.0.0.1" 1123 idB rqB
               :: IO KademliaHandle

        startRecvProcess khB

        send khA pB cmd
        (Answer sig) <- readChan (replyQueueDispatchChan rqB)

        closeK khA
        closeK khB

        return sig

    assert $ signalCommand sig == cmd
    assert $ nodePeer (signalSource sig) == pA
    assert $ nodeId   (signalSource sig) == idA

    return ()

-- | Make sure expect works the way it's supposed to
expectCheck :: Signal -> Ident -> Property
expectCheck sig idA = monadicIO $ do
    let rtM = rType (signalCommand sig)
    pre (isJust rtM)
    let (Just rt) = rtM
    let rr = ReplyRegistration [rt] (nodePeer (signalSource sig))

    replySig <- run $ do
        rqA <- emptyReplyQueue

        khA <- openOn "127.0.0.1" 1122 idA rqA

        startRecvProcess khA

        testChan <- newChan :: IO (Chan Reply)
        expect khA rr testChan
        dispatch (Answer sig) rqA

        (Answer replySig) <- readChan testChan :: IO Reply

        closeK khA

        return replySig

    assert $ replySig == sig

-- | Convert a command into a ReplyType
rType :: Command -> Maybe ReplyType
rType  PONG                  = Just  R_PONG
rType (RETURN_NODES _ nid _) = Just (R_RETURN_NODES nid)
rType _                      = Nothing

--------------------------------------------------------------------------------

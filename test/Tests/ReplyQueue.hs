--------------------------------------------------------------------------------

-- |
-- Module:      Tests.ReplyQueue
-- Description: Tests for DFINITY.Discovery.ReplyQueue
--
-- Tests specific to "DFINITY.Discovery.ReplyQueue".

--------------------------------------------------------------------------------

module Tests.ReplyQueue
  ( removedCheck
  , repliesCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.Chan      (Chan, getChanContents, newChan)
import           Control.Concurrent.STM       (atomically, readTVar)
import           Data.Maybe                   (isJust)

import           Test.QuickCheck              (Property)
import           Test.QuickCheck.Monadic      (assert, monadicIO, pre, run)

import           DFINITY.Discovery.ReplyQueue
                 (Reply (..), ReplyRegistration (..), ReplyType (..), dispatch,
                 emptyReplyQueue, register, replyQueueQueue)
import           DFINITY.Discovery.Types
                 (Command (..), Node (..), Signal (..))

--------------------------------------------------------------------------------

-- | Check whether registered reply handlers a used
repliesCheck :: Signal -> Signal -> Property
repliesCheck sig1 sig2 = monadicIO $ do
    let reg1 = toRegistration sig1
    let reg2 = toRegistration sig2

    pre $ isJust reg1 && isJust reg2

    let (Just replyReg1) = reg1
    let (Just replyReg2) = reg2

    contents <- run $ do
        rq <- emptyReplyQueue
        chan <- newChan :: IO (Chan Reply)

        register replyReg1 rq chan
        register replyReg2 rq chan

        dispatch (Answer sig1) rq
        dispatch (Answer sig2) rq

        contents <- getChanContents chan

        return contents

    assert . not . null $ contents

    let [reply1, reply2] = take 2 contents

    assert $ reply1 /= Closed
    assert $ reply2 /= Closed

    let (Answer unwrapped1) = reply1
    let (Answer unwrapped2) = reply2

    assert $ unwrapped1 == sig1
    assert $ unwrapped2 == sig2

-- | Check whether registered reply handlers are removed after usage
removedCheck :: Signal -> Property
removedCheck sig = monadicIO $ do
    let reg = toRegistration sig
    case reg of
        -- Discard the test case
        Nothing -> pre False
        Just reg' -> do
            removed <- run $ do
                rq <- emptyReplyQueue
                chan <- newChan :: IO (Chan Reply)
                register reg' rq chan
                dispatch (Answer sig) rq
                null <$> atomically (readTVar (replyQueueQueue rq))
            assert removed

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Signal -> Maybe ReplyRegistration
toRegistration sig = case rType (signalCommand sig) of
                        Nothing -> Nothing
                        Just rt -> Just (ReplyRegistration [rt] origin)
    where origin = nodePeer (signalSource sig)

          rType :: Command -> Maybe ReplyType
          rType  PONG                  = Just  R_PONG
          rType (RETURN_NODES _ nid _) = Just (R_RETURN_NODES nid)
          rType _                      = Nothing

--------------------------------------------------------------------------------

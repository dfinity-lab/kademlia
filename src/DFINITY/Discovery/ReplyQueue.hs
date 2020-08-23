--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.ReplyQueue
-- Description: A queue allowing to register handlers for expected replies
--
-- "DFINITY.Discovery.ReplyQueue" implements a queue designed for registering
-- handlers for expected replies.
--
-- The handlers are represented by unbound channels from the
-- "Control.Concurrent.Chan" module.

--------------------------------------------------------------------------------

module DFINITY.Discovery.ReplyQueue
  ( ReplyType (..)
  , ReplyRegistration (..)
  , Reply (..)
  , ReplyQueue (..)
  , emptyReplyQueue
  , emptyReplyQueueL
  , register
  , dispatch
  , expectedReply
  , flush
  ) where

--------------------------------------------------------------------------------

import qualified Control.Concurrent      as Conc
import           Control.Concurrent.Chan (Chan, newChan, writeChan)
import qualified Control.Concurrent.STM  as STM

import           Control.Monad           (forM_)
import           Data.List               (delete, find)
import           Data.Maybe              (isJust)

import           DFINITY.Discovery.Types
                 (Command (..), Ident, Node (nodePeer), Peer,
                 Signal (signalCommand, signalSource))
import           DFINITY.Discovery.Utils (threadDelay)

--------------------------------------------------------------------------------

-- |
-- The different types a replied signal could possibly have.
--
-- Note that these are only those 'Command' types which are replies to some
-- sort of request.
data ReplyType
  = R_PONG
  | R_RETURN_VALUE !Ident
  | R_RETURN_NODES !Ident
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- |
-- The representation of registered replies.
data ReplyRegistration
  = ReplyRegistration
    { replyTypes  :: ![ReplyType]
      -- ^ The types corresponding to each registered reply.
    , replyOrigin :: !Peer
      -- ^ The origin of the registered replies.
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- |
-- Convert a 'Signal' into its 'ReplyRegistration' representation.
toRegistration :: Reply -> Maybe ReplyRegistration
toRegistration Closed        = Nothing
toRegistration (Timeout reg) = Just reg
toRegistration (Answer sig)  = do
  rt <- case signalCommand sig of
          PONG                   -> Just R_PONG
          (RETURN_VALUE nid _)   -> Just (R_RETURN_VALUE nid)
          (RETURN_NODES _ nid _) -> Just (R_RETURN_NODES nid)
          _                      -> Nothing
  let origin = nodePeer (signalSource sig)
  pure (ReplyRegistration [rt] origin)

--------------------------------------------------------------------------------

-- |
-- Compare whether two 'ReplyRegistration's match
matchRegistrations :: ReplyRegistration -> ReplyRegistration -> Bool
matchRegistrations (ReplyRegistration rtsA idA) (ReplyRegistration rtsB idB)
  = (idA == idB) && (all (`elem` rtsA) rtsB || all (`elem` rtsB) rtsA)

--------------------------------------------------------------------------------

-- |
-- The actual type of a reply.
data Reply
  = Answer  !Signal
  | Timeout !ReplyRegistration
  | Closed
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- |
-- The actual type representing a reply queue.
data ReplyQueue
  = ReplyQueue
    { replyQueueQueue        :: !(STM.TVar [ExpectedResponse])
      -- ^ Queue of expected responses
    , replyQueueDispatchChan :: !(Chan Reply)
      -- ^ Channel for initial receiving of messages.
      --   Messages from this channel will be dispatched (via @dispatch@)
    , replyQueueRequestChan  :: !(Chan Reply)
      -- ^ This channel is needed for accepting requests from nodes.
      --   Only request will be processed, reply will be ignored.
    , replyQueueLogInfo      :: !(String -> IO ())
      -- ^ A logging function at the @INFO@ level.
      --
      --   FIXME: switch to Text
    , replyQueueLogError     :: !(String -> IO ())
      -- ^ A logging function at the @ERROR@ level.
      --
      --   FIXME: switch to Text
    }
  deriving ()

--------------------------------------------------------------------------------

-- |
-- A data structure representing a pending response.
data ExpectedResponse
  = ExpectedResponse
    { expectedResponseRegistration :: !ReplyRegistration
      -- ^ The underlying 'ReplyRegistration'.
    , expectedResponseReplyChan    :: !(Chan Reply)
      -- ^ The channel on which the 'Reply' will arrive.
    , expectedResponseThreadID     :: !Conc.ThreadId
      -- ^ The thread that waits for the 'Reply'.
    }
  deriving (Eq)

--------------------------------------------------------------------------------

-- |
-- Create a new 'ReplyQueue'.
emptyReplyQueue :: IO ReplyQueue
emptyReplyQueue = emptyReplyQueueL (const $ pure ()) (const $ pure ())

--------------------------------------------------------------------------------

-- |
-- Create a new 'ReplyQueue' with loggers.
emptyReplyQueueL
  :: (String -> IO ())
  -> (String -> IO ())
  -> IO ReplyQueue
emptyReplyQueueL logInfo logError = do
  ReplyQueue
    <$> STM.atomically (STM.newTVar [])
    <*> newChan
    <*> newChan
    <*> pure logInfo
    <*> pure logError

--------------------------------------------------------------------------------

-- |
-- Register a channel as a handler for a reply.
register
  :: ReplyRegistration
  -> ReplyQueue
  -> Chan Reply
  -> IO ()
register reg rq chan = do
  tid <- timeoutThread reg rq
  STM.atomically $ do
    rQueue <- STM.readTVar $ replyQueueQueue rq
    let er = ExpectedResponse reg chan tid
    STM.writeTVar (replyQueueQueue rq) (rQueue ++ [er])
    -- FIXME: this is slow, use a growable vector

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
timeoutThread :: ReplyRegistration -> ReplyQueue -> IO Conc.ThreadId
timeoutThread reg rq = do
  Conc.forkIO $ do
    -- Wait 5 seconds
    threadDelay 5 -- FIXME: should be configurable?

    -- Remove the ReplyRegistration from the ReplyQueue
    -- TODO: What should be here?
    -- tid <- myThreadId

    -- Send Timeout signal
    writeChan (replyQueueDispatchChan rq) (Timeout reg)

--------------------------------------------------------------------------------

-- |
-- Dispatch a reply over a registered handler.
-- If there is no handler, dispatch it to the default one.
dispatch :: Reply -> ReplyQueue -> IO ()
dispatch reply rq = do
  let matches regA (ExpectedResponse regB _ _) = matchRegistrations regA regB

  -- Try to find a registration matching the reply
  result <- STM.atomically $ do
    rQueue <- STM.readTVar (replyQueueQueue rq)
    let registrationMaybe = do
          rr <- toRegistration reply
          find (matches rr) rQueue
    case registrationMaybe of
      Just registration -> do
        -- Remove registration from queue
        STM.writeTVar (replyQueueQueue rq) $ delete registration rQueue
        pure (Just registration)
      Nothing -> pure Nothing

  case result of
    -- FIXME: doc
    Just (ExpectedResponse reg chan tid) -> do
      replyQueueLogInfo rq (" -- dispatch reply "
                            ++ show reply
                            ++ ": in queue, "
                            ++ show reg)

      -- Kill the timeout thread
      Conc.killThread tid

      -- Send the reply
      writeChan chan reply

    -- Send the reply over the default channel
    Nothing -> do
      replyQueueLogInfo rq (" -- dispatch reply "
                            ++ show reply
                            ++ ": not in queue")
      writeChan (replyQueueRequestChan rq) reply

--------------------------------------------------------------------------------

-- |
-- FIXME: doc
expectedReply
  :: Reply
  -> ReplyQueue
  -> IO Bool
expectedReply reply rq = do
  let matches regA (ExpectedResponse regB _ _) = matchRegistrations regA regB
  case toRegistration reply of
    Just rr -> do
      q <- STM.readTVarIO (replyQueueQueue rq)
      pure (isJust (find (matches rr) q))
    Nothing -> pure False

--------------------------------------------------------------------------------

-- |
-- Send 'Closed' signal to all handlers and empty the 'ReplyQueue'.
flush :: ReplyQueue -> IO ()
flush rq = do
  rQueue <- STM.atomically $ do
    rQueue <- STM.readTVar (replyQueueQueue rq)
    STM.writeTVar (replyQueueQueue rq) []
    pure rQueue

  forM_ rQueue $ \(ExpectedResponse _ chan tid) -> do
    Conc.killThread tid
    writeChan chan Closed

--------------------------------------------------------------------------------

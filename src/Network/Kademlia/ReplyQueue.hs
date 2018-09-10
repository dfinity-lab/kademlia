{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-|
Module      : Network.Kademlia.ReplyQueue
Description : A queue allowing to register handlers for expected replies

Network.Kademlia.ReplyQueue implements a Queue designed for registering
handlers for expected replies.

The handlers are represented by unbound channels from Control.Concurrency.Chan.
-}

module Network.Kademlia.ReplyQueue
    ( ReplyType(..)
    , ReplyRegistration(..)
    , Reply(..)
    , ReplyQueue(..)
    , emptyReplyQueue
    , emptyReplyQueueL
    , register
    , dispatch
    , expectedReply
    , flush
    ) where

import           Control.Concurrent.Classy     (MonadConc (STM), Chan, ThreadId,
                                                atomically, fork, killThread,
                                                newChan, writeChan,
                                                readTVarConc)
import           Control.Concurrent.Classy.STM (TVar, newTVar, readTVar,
                                                writeTVar)
import           Control.Monad                 (forM_)
import           Data.List                     (delete, find)
import           Data.Maybe                    (isJust)

import           Network.Kademlia.Types        (Command (..), Node (..), Peer,
                                                Signal (..))
import           Network.Kademlia.Utils        (threadDelay)

-- | The different types a replied signal could possibly have.
--
--   Note that these are only those Command types, which are replies to some
--   sort of request. Therefore, most Command types aren't contained in this
--   type.
data ReplyType i = R_PONG
                 | R_RETURN_VALUE i
                 | R_RETURN_NODES i
                   deriving (Eq, Show)

-- | The representation of registered replies
data ReplyRegistration i = RR {
      replyTypes  :: [ReplyType i]
    , replyOrigin :: Peer
    } deriving (Eq, Show)

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Reply i a -> Maybe (ReplyRegistration i)
toRegistration Closed        = Nothing
toRegistration (Timeout reg) = Just reg
toRegistration (Answer sig)  = case rType . command $ sig of
    Nothing -> Nothing
    Just rt -> Just (RR [rt] origin)
  where
    origin = peer $ source sig

    rType :: Command i a -> Maybe (ReplyType i)
    rType  PONG                  = Just  R_PONG
    rType (RETURN_VALUE nid _)   = Just (R_RETURN_VALUE nid)
    rType (RETURN_NODES _ nid _) = Just (R_RETURN_NODES nid)
    rType _                      = Nothing

-- | Compare wether two ReplyRegistrations match
matchRegistrations :: (Eq i) => ReplyRegistration i -> ReplyRegistration i -> Bool
matchRegistrations (RR rtsA idA) (RR rtsB idB) =
    idA == idB && (all (`elem` rtsA) rtsB || all (`elem` rtsB) rtsA)

-- | The actual type of a replay
data Reply i a = Answer (Signal i a)
               | Timeout (ReplyRegistration i)
               | Closed
                 deriving (Eq, Show)

-- | The actual type representing a ReplyQueue
data ReplyQueue m i a
  = RQ
    { queue        :: (TVar (STM m) [(ReplyRegistration i, Chan m (Reply i a), ThreadId m)])
      -- ^ Queue of expected responses
    , dispatchChan :: Chan m (Reply i a)
      -- ^ Channel for initial receiving of messages.
      -- Messages from this channel will be dispatched (via @dispatch@)
    , requestChan  :: Chan m (Reply i a)
      -- ^ This channels needed for accepting requests from nodes.
      -- Only request will be processed, reply will be ignored.
    , logInfo      :: String -> m ()
    , logError     :: String -> m ()
    }


-- | Create a new ReplyQueue
emptyReplyQueue
  :: (MonadConc m) => m (ReplyQueue m i a)
emptyReplyQueue = emptyReplyQueueL (const $ pure ()) (const $ pure ())

-- | Create a new ReplyQueue with loggers
emptyReplyQueueL
  :: (MonadConc m) => (String -> m ()) -> (String -> m ()) -> m (ReplyQueue m i a)
emptyReplyQueueL logInfo logError =
    RQ <$> (atomically . newTVar $ []) <*> newChan <*> newChan <*> pure logInfo <*>
    pure logError

-- | Register a channel as handler for a reply
register
  :: (MonadConc m)
  => ReplyRegistration i
  -> ReplyQueue m i a
  -> Chan m (Reply i a)
  -> m ()
register reg rq chan = do
    tId <- timeoutThread reg rq
    atomically $ do
        rQueue <- readTVar $ queue rq
        writeTVar (queue rq) $ rQueue ++ [(reg, chan, tId)]

timeoutThread
  :: (MonadConc m)
  => ReplyRegistration i
  -> ReplyQueue m i a
  -> m (ThreadId m)
timeoutThread reg rq = fork $ do
    -- Wait 5 seconds
    threadDelay 5

    -- Remove the ReplyRegistration from the ReplyQueue
    -- TODO: What should be here?
    -- myTId <- myThreadId

    -- Send Timeout signal
    writeChan (dispatchChan rq) . Timeout $ reg

-- | Dispatch a reply over a registered handler. If there is no handler,
--   dispatch it to the default one.
dispatch :: (Show i, Eq i, Eq (Chan m (Reply i a)), MonadConc m) => Reply i a -> ReplyQueue m i a -> m ()
dispatch reply rq = do
    -- Try to find a registration matching the reply
    result <- atomically $ do
        rQueue <- readTVar . queue $ rq
        case toRegistration reply of
            Just repReg -> case find (matches repReg) rQueue of
                Just registration -> do
                    -- Remove registration from queue
                    writeTVar (queue rq) $ delete registration rQueue
                    return . Just $ registration

                Nothing -> return Nothing
            Nothing -> return Nothing
    case result of
        Just (reg, chan, tId) -> do
            logInfo rq (" -- dispatch reply " ++ show reply ++ ": in queue, " ++ show reg)
            -- Kill the timeout thread
            killThread tId

            -- Send the reply
            writeChan chan reply

        -- Send the reply over the default channel
        Nothing -> do
            logInfo rq (" -- dispatch reply " ++ show reply ++ ": not in queue")
            writeChan (requestChan rq) reply

    where matches regA (regB, _, _) = matchRegistrations regA regB

expectedReply
  :: (Show i, Eq i, MonadConc m)
  => Reply i a -> ReplyQueue m i a -> m Bool
expectedReply (toRegistration -> reply) rq
    | Just repReg <- reply = isJust . find (matches repReg) <$> (readTVarConc $ queue rq)
    | otherwise = pure False
  where
    matches regA (regB, _, _) = matchRegistrations regA regB

-- | Send Closed signal to all handlers and empty ReplyQueue
flush :: (MonadConc m) => ReplyQueue m i a -> m ()
flush rq = do
    rQueue <- atomically $ do
        rQueue <- readTVar . queue $ rq
        writeTVar (queue rq) []
        return rQueue

    forM_ rQueue $ \(_, chan, tId) -> do
        killThread tId
        writeChan chan Closed

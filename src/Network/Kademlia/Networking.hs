{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Kademlia.Networking
       ( KademliaHandle
       , kSock
       , replyQueue
       , openOn
       , openOnL
       , startRecvProcess
       , send
       , expect
       , closeK
       , logInfo
       , logError
       , logError'
       ) where

import           Control.Monad.Conc.Class    (MonadConc)
import           Control.Concurrent.Classy   (Chan, MVar, ThreadId, fork, isEmptyMVar,
                                              killThread, newChan, newEmptyMVar, readChan,
                                              takeMVar, tryPutMVar, writeChan, yield)
import           Control.Monad.Catch         (SomeException, MonadCatch (catch),
                                              MonadMask, finally, throwM)
import           Control.Monad               (forM_, forever, unless, void)
import qualified Data.ByteString             as BS
import           Network.Socket.Classy       (MonadNetwork)
import qualified Network.Socket.Classy       as S
-- import           Network.Socket              (AddrInfo (..), AddrInfoFlag (AI_PASSIVE),
--                                               Family (..), Socket,
--                                               SocketOption (ReuseAddr),
--                                               SocketType (Datagram), addrAddress,
--                                               addrFlags, bind, close, defaultHints,
--                                               defaultProtocol, getAddrInfo,
--                                               setSocketOption, socket, withSocketsDo)
-- import qualified Network.Socket.ByteString   as S
import           System.IO.Error             (ioError, userError)

import           Network.Kademlia.Config     (KademliaConfig (..), defaultConfig)
import           Network.Kademlia.Protocol   (parse, serialize)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (dispatchChan),
                                              ReplyRegistration, flush, register)
import           Network.Kademlia.Types      (Command, Peer (..), Serialize (..), toPeer)

-- | A handle to a UDP socket running the Kademlia connection
data KademliaHandle m i a = KH {
      kSock      :: S.Socket m
    , sendThread :: ThreadId m
    , sendChan   :: Chan m (Command i a, Peer)
    , replyQueue :: ReplyQueue m i a
    , recvThread :: MVar m (ThreadId m)
    , logInfo    :: String -> m ()
    , logError   :: String -> m ()
    }

logError' :: KademliaHandle m i a -> SomeException -> m ()
logError' h = logError h . show

openOn
  :: (Show i, Serialize i, Serialize a, MonadNetwork m, MonadConc m)
  => String
  -> String
  -> i
  -> ReplyQueue m i a
  -> m (KademliaHandle m i a)
openOn host port id' rq = openOnL host port id' lim rq (const $ pure ()) (const $ pure ())
  where
    lim = msgSizeLimit defaultConfig

-- | Open a Kademlia connection on specified port and return a corresponding
--   KademliaHandle
openOnL
  :: (Show i, Serialize i, Serialize a, MonadNetwork m, MonadConc m)
  => String
  -> String
  -> i
  -> Int
  -> ReplyQueue m i a
  -> (String -> m ())
  -> (String -> m ())
  -> m (KademliaHandle m i a)
openOnL host port id' lim rq logInfo logError = S.withSocketsDo $ do
    -- Get addr to bind to
    serveraddrs <- S.getAddrInfo
                 (Just (S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }))
                 (Just host)
                 (Just port)

    -- TODO: support IPV6 by binding to two sockets
    let serveraddr = head $ filter (\a -> S.addrFamily a == S.AF_INET) serveraddrs

    -- Create socket and bind to it
    sock <- S.socket (S.addrFamily serveraddr) S.Datagram S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1
    S.bind sock (S.addrAddress serveraddr)

    chan <- newChan
    tId <- fork $ sendProcessL sock lim id' chan logInfo logError
    mvar <- newEmptyMVar

    -- Return the handle
    return $ KH sock tId chan rq mvar logInfo logError

sendProcessL
  :: forall m i a.
     ( Show i, Serialize i, Serialize a
     , MonadNetwork m, MonadConc m, MonadMask m )
  => S.Socket m
  -> Int
  -> i
  -> Chan m (Command i a, Peer)
  -> (String -> m ())
  -> (String -> m ())
  -> m ()
sendProcessL sock lim nid chan logInfo logError =
    (S.withSocketsDo . forever . (`catch` logSomeError') . void $ do
        pair@(cmd, Peer host port) <- readChan chan

        logInfo $ "Send process: sending .. " ++ show pair ++ " (id " ++ show nid ++ ")"
        -- Get Peer's address
        (peeraddr:_) <- S.getAddrInfo Nothing (Just host) (Just . show $ port)

        -- Send the signal
        case serialize lim nid cmd of
            Left err   -> logError err
            Right sigs -> forM_ sigs $ \sig -> S.sendTo sock sig (S.addrAddress peeraddr))
    -- Close socket on exception (ThreadKilled)
    `finally` S.close sock
  where
    logSomeError' :: SomeException -> m ()
    logSomeError' e = logError $ "Caught error " ++ show e

-- | Dispatch the receiving process
--
--   Receive a signal and first try to dispatch it via the ReplyQueue. If that
--   fails, send it to the supplied default channel instead.
--
--   This throws an exception if called a second time.
startRecvProcess
    :: (Show i, Serialize i, Serialize a, MonadNetwork m, MonadConc m, MonadMask m)
    => KademliaHandle m i a
    -> m ()
startRecvProcess kh = do
    tId <- fork $ (S.withSocketsDo . forever $ do
        -- Read from socket
        (received, addr) <- S.recvFrom (kSock kh) 1500
        -- Try to create peer
        peer <- toPeer addr
        case peer of
            Nothing -> logError kh ("Unknown peer " ++ show addr)
            Just p  ->
                -- Try parsing the signal
                case parse p received of
                    Left _    ->
                      logError kh ("Can't parse " ++ show (BS.length received) ++ " bytes from " ++ show peer)
                    Right sig -> do
                        logInfo kh ("Received signal " ++ show sig ++ " from " ++ show p)
                        -- Send the signal to the receivng process of instance
                        writeChan (dispatchChan . replyQueue $ kh) $ Answer sig
                        logInfo kh (" -- added from signal " ++ show p ++ " to chan")
        )
            -- Send Closed reply to all handlers
            `finally` do
                flush . replyQueue $ kh
                writeChan (dispatchChan . replyQueue $ kh) Closed

    success <- tryPutMVar (recvThread kh) tId
    unless success . throwM . userError $ "Receiving process already running"

-- | Send a Signal to a Peer over the connection corresponding to the
--   KademliaHandle
send
  :: (MonadNetwork m, MonadConc m)
  => KademliaHandle m i a
  -> Peer
  -> Command i a
  -> m ()
send kh peer cmd = writeChan (sendChan kh) (cmd, peer)

-- | Register a handler channel for a Reply
expect
  :: (MonadConc m)
  => KademliaHandle m i a
  -> ReplyRegistration i
  -> Chan m (Reply i a)
  -> m ()
expect kh reg = register reg . replyQueue $ kh

-- | Close the connection corresponding to a KademliaHandle
closeK :: (MonadNetwork m, MonadConc m) => KademliaHandle m i a -> m ()
closeK kh = do
    -- Kill recvThread
    empty <- isEmptyMVar . recvThread $ kh
    unless empty $ do
        tId <- takeMVar . recvThread $ kh
        killThread tId

    -- Kill sendThread
    killThread . sendThread $ kh

    S.close $ kSock kh
    yield

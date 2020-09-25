--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Networking
-- Description: All of the UDP network code
--
-- "DFINITY.Discovery.Networking" implements all the UDP network functionality.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Networking
  ( KademliaHandle
  , handleSocket
  , handleReplyQueue
  , openOn
  , openOnL
  , startRecvProcess
  , send
  , expect
  , closeK
  , handleLogInfo
  , handleLogError
  , handleLogError'
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent
                 (ThreadId, forkIO, killThread, yield)
import           Control.Concurrent.Chan
                 (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar
                 (MVar, isEmptyMVar, newEmptyMVar, takeMVar, tryPutMVar)
import           Control.Exception            (SomeException, catch, finally)
import           Control.Monad                (forM_, forever, unless, void)
import qualified Data.ByteString              as BS
import           Network.Socket
                 (AddrInfo (..), AddrInfoFlag (AI_PASSIVE), Family (..),
                 Socket, SocketOption (ReuseAddr), SocketType (Datagram),
                 addrAddress, addrFlags, bind, close, defaultHints,
                 defaultProtocol, getAddrInfo, setSocketOption, socket,
                 withSocketsDo)
import qualified Network.Socket.ByteString    as S
import           Data.IP                      (IP)
import           Network.Socket               (PortNumber)

import           DFINITY.Discovery.Config
                 (KademliaConfig (..), defaultConfig)
import           DFINITY.Discovery.Protocol   (parse, serialize)
import           DFINITY.Discovery.ReplyQueue
                 (Reply (..), ReplyQueue (replyQueueDispatchChan),
                 ReplyRegistration, flush, register)
import           DFINITY.Discovery.Types
                 (Command, Ident, Peer (..), toPeer)

--------------------------------------------------------------------------------

-- | A handle to a UDP socket running the Kademlia connection
data KademliaHandle
  = KademliaHandle
    { handleSocket     :: !Socket
    , handleSendThread :: !ThreadId
    , handleSendChan   :: !(Chan (Command, Peer))
    , handleReplyQueue :: !ReplyQueue
    , handleRecvThread :: !(MVar ThreadId)
    , handleLogInfo    :: !(String -> IO ())
    , handleLogError   :: !(String -> IO ())
    }
  deriving ()

--------------------------------------------------------------------------------

handleLogError'
  :: KademliaHandle
  -> SomeException
  -> IO ()
handleLogError' h = handleLogError h . show

--------------------------------------------------------------------------------

openOn
  :: IP
  -> PortNumber
  -> Ident
  -> ReplyQueue
  -> IO KademliaHandle
openOn host port id' rq
  = let lim = configMsgSizeLimit defaultConfig
    in openOnL host port id' lim rq (const (pure ())) (const (pure ()))

--------------------------------------------------------------------------------

-- |
-- Open a Kademlia connection on specified port and return a corresponding
-- 'KademliaHandle'.
openOnL
  :: IP
  -> PortNumber
  -> Ident
  -> Int
  -> ReplyQueue
  -> (String -> IO ())
  -> (String -> IO ())
  -> IO KademliaHandle
openOnL host port id' lim rq logInfo logError = withSocketsDo $ do
  -- Get address to bind to
  serveraddrs <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 (Just (show host))
                 (Just (show port))

  -- TODO: support IPV6 by binding to two sockets
  let serveraddr = head $ filter (\a -> addrFamily a == AF_INET) serveraddrs

  -- Create socket and bind to it
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)

  chan <- newChan
  tid <- forkIO $ sendProcessL sock lim id' chan logInfo logError
  mvar <- newEmptyMVar

  -- Return the handle
  pure $ KademliaHandle sock tid chan rq mvar logInfo logError

--------------------------------------------------------------------------------

sendProcessL
  :: Socket
  -> Int
  -> Ident
  -> Chan (Command, Peer)
  -> (String -> IO ())
  -> (String -> IO ())
  -> IO ()
sendProcessL sock lim nid chan logInfo logError = do
  let logSomeError' :: SomeException -> IO ()
      logSomeError' e = logError $ "Caught error " ++ show e

  -- Close socket on exception (ThreadKilled)
  (`finally` close sock) $ do
    withSocketsDo $ forever $ do
      (`catch` logSomeError') $ void $ do
        pair@(cmd, Peer host port) <- readChan chan

        logInfo ("Send process: sending .. "
                 ++ show pair
                 ++ " (id " ++ show nid ++ ")")

        -- Get Peer's address
        -- FIXME: partial pattern match
        (peeraddr : _) <- getAddrInfo
                          Nothing
                          (Just (show host))
                          (Just (show port))

        -- Send the signal
        let signals = serialize lim nid cmd
        let addr = addrAddress peeraddr
        forM_ signals $ \signal -> do
          S.sendTo sock signal addr

--------------------------------------------------------------------------------

-- | Dispatch the receiving process
--
--   Receive a signal and first try to dispatch it via the ReplyQueue. If that
--   fails, send it to the supplied default channel instead.
--
--   This throws an exception if called a second time.
startRecvProcess
  :: KademliaHandle
  -> IO ()
startRecvProcess kh = do
  let finalize = do
        flush $ handleReplyQueue kh
        writeChan (replyQueueDispatchChan (handleReplyQueue kh)) Closed

  tid <- forkIO $ (`finally` finalize) $ withSocketsDo $ forever $ do
    -- Read from socket
    (received, addr) <- S.recvFrom (handleSocket kh) 1500
    -- Try to create peer
    peer <- toPeer addr
    case peer of
      Nothing -> handleLogError kh ("Unknown peer " ++ show addr)
      Just p  -> do
        -- Try parsing the signal
        case parse received of
          Left _ -> do
            handleLogError kh ("Can't parse "
                               ++ show (BS.length received)
                               ++ " bytes from "
                               ++ show peer)
          Right f -> do
            let sig = f p
            handleLogInfo kh ("Received signal "
                              ++ show sig
                              ++ " from "
                              ++ show p)
            -- Send the signal to the receivng process of instance
            writeChan (replyQueueDispatchChan (handleReplyQueue kh)) (Answer sig)
            handleLogInfo kh (" -- added from signal "
                              ++ show p
                              ++ " to chan")

  success <- tryPutMVar (handleRecvThread kh) tid
  unless success $ ioError $ userError "Receiving process already running"

--------------------------------------------------------------------------------

-- |
-- Send a Signal to a Peer over the connection corresponding to the
-- 'KademliaHandle'.
send
  :: KademliaHandle
  -> Peer
  -> Command
  -> IO ()
send kh peer cmd = writeChan (handleSendChan kh) (cmd, peer)

--------------------------------------------------------------------------------

-- | Register a handler channel for a 'Reply'.
expect
  :: KademliaHandle
  -> ReplyRegistration
  -> Chan Reply
  -> IO ()
expect kh reg = register reg . handleReplyQueue $ kh

--------------------------------------------------------------------------------

-- | Close the connection corresponding to a 'KademliaHandle'.
closeK
  :: KademliaHandle
  -> IO ()
closeK kh = do
  -- Kill recvThread
  empty <- isEmptyMVar (handleRecvThread kh)
  unless empty $ do
    tid <- takeMVar (handleRecvThread kh)
    killThread tid

  -- Kill sendThread
  killThread (handleSendThread kh)

  close $ handleSocket kh
  yield

--------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Network.Socket.Classy
  ( module Network.Socket.Classy
  , Socket.AddrInfo (..)
  , Socket.AddrInfoFlag (..)
  , Socket.defaultHints
  , Socket.defaultProtocol
  , Socket.Family (..)
  , Socket.SocketOption (..)
  , Socket.SocketType (..)
  , Socket.HostName
  , Socket.ServiceName
  , Socket.PortNumber
  , Socket.SockAddr (..)
  ) where

import qualified Network.Socket as Socket

import qualified Data.ByteString as BS

class (Monad m) => MonadNetwork m where
  type Socket m :: *
  withSocketsDo :: m a -> m a
  getAddrInfo
    :: Maybe Socket.AddrInfo
    -> Maybe Socket.HostName
    -> Maybe Socket.ServiceName
    -> m [Socket.AddrInfo]
  connect
    :: Socket m
    -> Socket.SockAddr
    -> m ()
  bind
    :: Socket m
    -> Socket.SockAddr
    -> m ()
  listen
    :: Socket m
    -> Int
    -> m ()
  accept
    :: Socket m
    -> m (Socket m, Socket.SockAddr)
  close
    :: Socket m
    -> m ()
  close'
    :: Socket m
    -> m ()
  shutdown
    :: Socket m
    -> Socket.ShutdownCmd
    -> m ()
  socket
    :: Socket.Family
    -> Socket.SocketType
    -> Socket.ProtocolNumber
    -> m (Socket m)
  getSocketOption
    :: Socket m
    -> Socket.SocketOption
    -> m Int
  setSocketOption
    :: Socket m
    -> Socket.SocketOption
    -> Int
    -> m ()
  inet_ntoa
    :: Socket.HostAddress
    -> m String
  send
    :: Socket m
    -> BS.ByteString
    -> m Int
  sendAll
    :: Socket m
    -> BS.ByteString
    -> m ()
  sendTo
    :: Socket m
    -> BS.ByteString
    -> Socket.SockAddr
    -> m Int
  sendAllTo
    :: Socket m
    -> BS.ByteString
    -> Socket.SockAddr
    -> m ()
  sendMany
    :: Socket m
    -> [BS.ByteString]
    -> m ()
  sendManyTo
    :: Socket m
    -> [BS.ByteString]
    -> Socket.SockAddr
    -> m ()
  recv
    :: Socket m
    -> Int
    -> m BS.ByteString
  recvFrom
    :: Socket m
    -> Int
    -> m (BS.ByteString, Socket.SockAddr)

  -- socketToHandle -- this might be problematic
  --   :: Socket m
  --   -> IOMode
  --   -> m Handle

-- AddrInfo
-- AddrInfoFlag
-- Family
-- HostName
-- PortNumber
-- ServiceName
-- SockAddr
-- Socket
-- SocketType
-- accept
-- bind
-- close
-- connect
-- defaultHints
-- getAddrInfo
-- getSocketName
-- inet_ntoa
-- listen
-- socket
-- socketPort
-- socketToHandle

instance MonadNetwork IO

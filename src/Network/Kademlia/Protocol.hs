--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Network.Kademlia.Protocol
-- Description : Implementation of the actual protocol
--
-- "Network.Kademlia.Protocol" implements the parsing and serialization of
-- 'ByteString's into values of type 'Command'.

--------------------------------------------------------------------------------

module Network.Kademlia.Protocol
  ( serialize
  , parse
  ) where

--------------------------------------------------------------------------------

import           Network.Kademlia.Protocol.Parse     (parse)
import           Network.Kademlia.Protocol.Serialize (serialize)

--------------------------------------------------------------------------------

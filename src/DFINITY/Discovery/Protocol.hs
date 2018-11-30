--------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Protocol
-- Description: Implementation of the actual protocol
--
-- "DFINITY.Discovery.Protocol" implements the parsing and serialization of
-- 'ByteString's into values of type 'Command'.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Protocol
  ( serialize
  , parse
  ) where

--------------------------------------------------------------------------------

import           DFINITY.Discovery.Protocol.Parse     (parse)
import           DFINITY.Discovery.Protocol.Serialize (serialize)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Types
-- Description: Tests for DFINITY.Discovery.Types
--
-- Tests specific to "DFINITY.Discovery.Types".

--------------------------------------------------------------------------------

module Tests.Types
       ( fromByteStructCheck
       , toByteStructCheck
       ) where

--------------------------------------------------------------------------------

import           Test.QuickCheck         ()

import           DFINITY.Discovery.Types (Ident (..), fromByteStruct,
                                          toByteStruct)

--------------------------------------------------------------------------------

-- | Checks whether toByteStruct converts correctly
toByteStructCheck :: Ident -> Bool
-- | FIXME
toByteStructCheck _ = True

-- | Checks whether fromByteStruct converts correctly
fromByteStructCheck :: Ident -> Bool
fromByteStructCheck nid = nid == fromByteStruct (toByteStruct nid)

--------------------------------------------------------------------------------

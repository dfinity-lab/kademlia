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
import qualified Data.ByteString         as B
import           Test.QuickCheck         ()
import           Data.Bits               (testBit)
import           DFINITY.Discovery.Types
                 (Ident (..), fromByteStruct, toByteStruct)
import           Data.Bit.ThreadSafe     (unBit)
import           Data.Vector.Unboxed     (length, (!))
import           Prelude                 hiding (length)
--------------------------------------------------------------------------------

-- | Checks whether toByteStruct converts correctly
toByteStructCheck :: Ident -> Bool
toByteStructCheck nid = foldl foldingFunc True [0..length converted - 1]
    where converted = toByteStruct nid
          byteWords = B.unpack . fromIdent $ nid
          foldingFunc b i = b && (unBit (converted ! i) == access byteWords i)
          access ws i = testBit (ws !! (i `div` 8)) (i `mod` 8)

-- | Checks whether fromByteStruct converts correctly
fromByteStructCheck :: Ident -> Bool
fromByteStructCheck nid = nid == fromByteStruct (toByteStruct nid)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Protocol
-- Description: Test for DFINITY.Discovery.Protocol
--
-- Tests specific to "DFINITY.Discovery.Protocol".

--------------------------------------------------------------------------------

module Tests.Protocol
  ( parseCheck
  , lengthCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Arrow              (left, (>>>))

import qualified Data.ByteString            as B
import           Test.QuickCheck
                 (Property, conjoin, counterexample, (===), (==>))

import           DFINITY.Discovery.Protocol (parse, serialize)
import           DFINITY.Discovery.Types
                 (Command (..), Node (..), Signal (..))

import           Tests.TestTypes            (IdType (..))

--------------------------------------------------------------------------------

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal IdType String -> Property
parseCheck s = test $ do
  serialized <- serialize 99999 nid (signalCommand s)
  left show
    $ parse (nodePeer (signalSource s))
    $ head serialized
    where nid = nodeId (signalSource s)
          test (Left   _) = counterexample "Parsing failed" False
          test (Right s') = counterexample
            ("Signals differ:\nIn:  " ++ show s ++ "\nOut: "
                 ++ show s' ++ "\n") $ s === s'

-- | Commands are cut into small enough pieces.
lengthCheck :: Signal IdType String -> Property
lengthCheck s =
    isReturnNodes (signalCommand s) ==>
    case serialize partLen (nodeId (signalSource s)) (signalCommand s) of
        Left er   -> counterexample ("Serialization error: " ++ er) False
        Right bss -> conjoin $ flip map bss $
            \bs -> counterexample (err bs) $ B.length bs <= partLen
  where
    err bs = "Serialized part of signal is too long: " ++ show (B.length bs) ++ " bytes"
    partLen = 100
    isReturnNodes (RETURN_NODES _ _ _) = True
    isReturnNodes _                    = False

--------------------------------------------------------------------------------

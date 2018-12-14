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

import           Control.Arrow              (left)

import qualified Data.ByteString            as B
import           Test.QuickCheck
                 (Property, conjoin, counterexample, (===), (==>))

import           DFINITY.Discovery.Protocol (parse, serialize)
import           DFINITY.Discovery.Types
                 (Command (..), Node (..), Signal (..))

--------------------------------------------------------------------------------

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal -> Property
parseCheck s = do
  let nid = nodeId (signalSource s)
  let test (Left   _) = counterexample "Parsing failed" False
      test (Right s') = counterexample ("Signals differ:"
                                        ++ "\nIn:  " ++ show s
                                        ++ "\nOut: " ++ show s' ++ "\n")
                        $ s === s'
  test
    $ left show
    $ fmap (\f -> f (nodePeer (signalSource s)))
    $ parse (serialize 99999 nid (signalCommand s))

-- | Commands are cut into small enough pieces.
lengthCheck :: Signal -> Property
lengthCheck s =
    isReturnNodes (signalCommand s) ==>
    let bss = [serialize partLen (nodeId (signalSource s)) (signalCommand s)]
    in conjoin $ flip map bss $ \bs -> counterexample (err bs) $ B.length bs <= partLen
  where
    err bs = "Serialized part of signal is too long: " ++ show (B.length bs) ++ " bytes"
    partLen = 100
    isReturnNodes (RETURN_NODES _ _ _) = True
    isReturnNodes _                    = False

--------------------------------------------------------------------------------

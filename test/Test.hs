--------------------------------------------------------------------------------

-- |
-- Module: Tests
-- Description: Tests for the modules
--
-- A few tests using QuickCheck and Tasty to make sure everything works
-- the way it's supposed to.

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      as HU
import           Test.Tasty.QuickCheck as QC

import           Tests.Implementation
                 (idClashCheck, joinBannedCheck, joinCheck, joinFullCheck,
                 lookupNodesCheck, nodeDownCheck)
import           Tests.Instance
                 (banNodeCheck, handlesPingCheck, isNodeBannedCheck,
                 snapshotCheck, trackingKnownPeersCheck)
import           Tests.Networking      (expectCheck, sendCheck)
import           Tests.Protocol        (lengthCheck, parseCheck)
import           Tests.ReplyQueue      (removedCheck, repliesCheck)
import           Tests.Tree
                 (bucketSizeCheck, deleteCheck, findClosestCheck, insertCheck,
                 pickupNotClosestDifferentCheck, refreshCheck, splitCheck,
                 viewCheck)
import           Tests.Types           (fromByteStructCheck, toByteStructCheck)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests, hUnitTests]

quickCheckTests :: TestTree
quickCheckTests = testGroup "QuickCheck"
    [ typeChecks
    , protocolChecks
    , networkingChecks
    , treeChecks
    , instanceChecks
    , replyQueueChecks
    , implementationChecks
    ]

typeChecks :: TestTree
typeChecks = testGroup "DFINITY.Discovery.Types" [
      QC.testProperty "ByteString to ByteStruct conversion works"
         toByteStructCheck
    , QC.testProperty "ByteStruct to ByteString conversion works"
         fromByteStructCheck
    ]

protocolChecks :: TestTree
protocolChecks = testGroup "DFINITY.Discovery.Protocol" [
      QC.testProperty "Protocol Serializing and Parsing works"
         parseCheck
    , QC.testProperty "Protocol messages are cut in pieces of required size"
         lengthCheck
    ]

networkingChecks :: TestTree
networkingChecks = testGroup "DFINITY.Discovery.Networking" [
      QC.testProperty "Sending and Receiving works"
         sendCheck
    , QC.testProperty "Expecting works the way it's supposed to"
         expectCheck
    ]

treeChecks :: TestTree
treeChecks = testGroup "DFINITY.Discovery.Tree" [
      QC.testProperty "Inserting into the Tree works"
         insertCheck
    , QC.testProperty "Deleting from the Tree works"
         deleteCheck
    , QC.testProperty "Splitting works as expected"
         splitCheck
    , QC.testProperty "Buckets are within the size limit"
         bucketSizeCheck
    , QC.testProperty "Refreshing works as expected"
         refreshCheck
    , QC.testProperty "Finding closest works"
         findClosestCheck
    , QC.testProperty "PickupRandom with closest provided doesn't contain closest"
         pickupNotClosestDifferentCheck
    , QC.testProperty "Getting view of tree works correctly"
         viewCheck
    ]

instanceChecks :: TestTree
instanceChecks = testGroup "DFINITY.Discovery.Instance"
    [ QC.testProperty "Peers are put into the tree on first encounter"
         trackingKnownPeersCheck
    , HU.testCase "Setting ban and checking ban status works"
         isNodeBannedCheck
    , HU.testCase "Messages from banned node are ignored"
         banNodeCheck
    , QC.testProperty "Kademlia state snapshot is serialized and deserialized well"
         snapshotCheck
    ]

replyQueueChecks :: TestTree
replyQueueChecks = testGroup "DFINITY.Discovery.ReplyQueue" [
      QC.testProperty "Registering replies works"
          repliesCheck
    , QC.testProperty "Registrations are removed after being dispatched"
         removedCheck
    ]

implementationChecks :: TestTree
implementationChecks = testGroup "DFINITY.Discovery.Implementation" [
       QC.testProperty "Joining the Network works"
         joinCheck
     , QC.testProperty "Joining the Network full works"
         joinFullCheck
     , QC.testProperty "ID clashes are detected"
         idClashCheck
     , QC.testProperty "Join network to banned node works"
         joinBannedCheck
     , QC.testProperty "Looking up Nodes works"
        lookupNodesCheck
    ]

hUnitTests :: TestTree
hUnitTests = testGroup "HUnit" [
      instanceCases
    , implementationCases
    ]

instanceCases :: TestTree
instanceCases = testGroup "DFINITY.Discovery.Instance" [
      HU.testCase "PINGs are automaticly handled"
         handlesPingCheck
    ]

implementationCases :: TestTree
implementationCases = testGroup "DFINITY.Discovery.Implementation" [
      HU.testCase "Trying to join over an offline Node is detected"
         nodeDownCheck
    ]

--------------------------------------------------------------------------------

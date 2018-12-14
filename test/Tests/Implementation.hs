--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Implementation
-- Description: Tests for DFINITY.Discovery.Implementation
--
-- Tests specific to "DFINITY.Discovery.Implementation".

--------------------------------------------------------------------------------

module Tests.Implementation
  ( joinCheck
  , joinFullCheck
  , lookupNodesCheck
  , nodeDownCheck
  , joinBannedCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent.STM     (atomically, readTVar)
import           Control.Monad              (forM, forM_, mapM, zipWithM)
import qualified Data.ByteString.Char8      as BSC8

import           Test.HUnit                 (Assertion, assertEqual)
import           Test.QuickCheck            (Property)
import           Test.QuickCheck.Monadic    (PropertyM, assert, monadicIO, run)

import qualified DFINITY.Discovery          as K
import           DFINITY.Discovery.Config   (defaultK, defaultRoutingSharingN)
import           DFINITY.Discovery.Instance
                 (BanState (..), KademliaInstance (..), KademliaState (..))
import qualified DFINITY.Discovery.Tree     as T
import           DFINITY.Discovery.Types    (Ident (..), Node (..), Peer (..))

import           Tests.TestTypes            (IdBunch (..))

--------------------------------------------------------------------------------

constructNetwork
  :: IdBunch
  -> PropertyM IO [KademliaInstance]
constructNetwork idBunch = run $ do
  let ids = getIds idBunch
  let entryNode = Node (Peer "127.0.0.1" 3123) (head ids)
  let createInst port = K.create ("127.0.0.1", port) ("127.0.0.1", port)
  instances <- zipWithM createInst [3123..] ids
               :: IO [KademliaInstance]
  forM_ (tail instances) $ \inst -> do
    K.joinNetwork inst (nodePeer entryNode)
  pure instances

--------------------------------------------------------------------------------

joinNetworkVerifier
  :: Int
  -> IdBunch
  -> Property
joinNetworkVerifier bucketThreshold idBunch = monadicIO $ do
  let isBucketFilled inst = do
        tree <- atomically (readTVar (stateTree (instanceState inst)))
        let treeLen = length (T.toList tree)
        pure (treeLen >= bucketThreshold)
  instances <- constructNetwork idBunch
  present <- run $ do
    mapM_ K.close instances
    mapM isBucketFilled instances
  assert (and present)

--------------------------------------------------------------------------------

-- | Checks that nodes contain at least @k@ neighbours in their buckets
joinCheck
  :: IdBunch
  -> Property
joinCheck = joinNetworkVerifier defaultK

--------------------------------------------------------------------------------

-- |
-- Checks that nodes from a 'RETURN_NODES' request were added to bucket:
-- [CSL-258] and [CSL-260]
-- Thus node should contain at least @k + k/2@ nodes.
joinFullCheck
  :: IdBunch
  -> Property
joinFullCheck = joinNetworkVerifier (defaultK + defaultRoutingSharingN)

--------------------------------------------------------------------------------

-- | Make sure an offline peer is detected
nodeDownCheck :: Assertion
nodeDownCheck = do
  let idA = Ident (BSC8.pack "hello")
  let idB = Ident (BSC8.pack "salve")
  let entryNode = Node (Peer "127.0.0.1" 1124) idB
  inst <- K.create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idA
  joinResult <- K.joinNetwork inst (nodePeer entryNode)
  K.close inst

  assertEqual "" joinResult K.NodeDown

--------------------------------------------------------------------------------

-- | Make sure banNode works correctly
joinBannedCheck :: Ident -> Ident -> Property
joinBannedCheck idA idB = monadicIO $ do
  let entryNode = Node (Peer "127.0.0.1" 1124) idB

  joinResult <- run $ do
    inst <- K.create ("127.0.0.1", 1123) ("127.0.0.1", 1123) idA

    K.banNode inst entryNode BanForever
    joinResult <- K.joinNetwork inst (nodePeer entryNode)

    K.close inst

    pure joinResult

  assert (joinResult == K.NodeBanned)

--------------------------------------------------------------------------------

lookupNodesCheck :: IdBunch -> Property
lookupNodesCheck ids = monadicIO $ do
  let check :: Ident -> Maybe Node -> Bool
      check nid = maybe False ((== nid) . nodeId)
  let tryLookup inst nid = check nid <$> K.lookupNode inst nid

  instances <- constructNetwork ids

  success <- run $ do
    s <- forM instances $ \inst -> do
      and <$> mapM (tryLookup inst) (getIds ids)

    mapM_ K.close instances

    pure s

  assert (and success)

--------------------------------------------------------------------------------

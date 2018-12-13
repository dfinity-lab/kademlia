--------------------------------------------------------------------------------

-- |
-- Module:      Tests.Tree
-- Description: Tests for DFINITY.Discovery.Tree
--
-- Tests specific to "DFINITY.Discovery.Tree".

--------------------------------------------------------------------------------

module Tests.Tree
  ( withTree
  , bucketSizeCheck
  , deleteCheck
  , findClosestCheck
  , insertCheck
  , lookupCheck
  , pickupNotClosestDifferentCheck
  , refreshCheck
  , splitCheck
  , viewCheck
  ) where

--------------------------------------------------------------------------------

import           Control.Monad            (filterM, join)
import           Data.Foldable            (foldrM)
import           Data.Function            (on)
import           Data.List                (sort, sortBy)
import           Data.Maybe               (isJust)
import           Data.Ord                 (comparing)
import qualified Data.Set                 as S
import           System.Random            (mkStdGen)
import           Test.QuickCheck
                 (Property, conjoin, counterexample, property)

import           DFINITY.Discovery.Config
                 (WithConfig, configK, defaultConfig, usingConfig)
import qualified DFINITY.Discovery.Tree   as T
import           DFINITY.Discovery.Types
                 (Node (..), Serialize (..), Timestamp, distance)

import           Tests.TestTypes          (IdType (..), NodeBunch (..))

--------------------------------------------------------------------------------

usingDefaultConfig :: WithConfig i a -> a
usingDefaultConfig = flip usingConfig defaultConfig

-- | Helper method for lookup checking
lookupCheck :: (Serialize i, Eq i) => T.NodeTree i -> Node i -> Bool
lookupCheck tree node = usingDefaultConfig (T.lookup tree (nodeId node)) == Just node

-- | Check whether an inserted Node is retrievable
insertCheck :: IdType -> Node IdType -> Bool
insertCheck nid node = usingDefaultConfig $ do
    let timestamp = 0 :: Timestamp
    tree <- join (T.insert
                  <$> pure (T.create nid)
                  <*> pure node
                  <*> pure timestamp)
    return $ lookupCheck tree node

-- | Make sure a deleted Node can't be retrieved anymore
deleteCheck :: IdType -> Node IdType -> Bool
deleteCheck nid node = usingDefaultConfig $ do
    let timestamp = 0 :: Timestamp
    origin <- join (T.insert
                    <$> pure (T.create nid)
                    <*> pure node
                    <*> pure timestamp)
    tree <- T.delete origin (nodePeer node)
    pure (not (lookupCheck tree node))

withTree :: (T.NodeTree IdType -> [Node IdType] -> WithConfig IdType a) ->
            NodeBunch IdType -> IdType -> a
withTree f bunch nid = usingDefaultConfig $ do
    let timestamp = 0 :: Timestamp
    tree <- join (foldrM (\node tree -> T.insert tree node timestamp)
                  <$> pure (T.create nid)
                  <*> pure (nodes bunch))
    f tree (nodes bunch)

splitCheck :: NodeBunch IdType -> IdType -> Property
splitCheck = withTree (\tree -> pure . conjoin . foldr (foldingFunc tree) [])
  where
    contains tree node = node `elem` T.toList tree

    foldingFunc tree node props = prop : props
      where
        prop = counterexample ("Failed to find " ++ show node) (
          -- There is the possibility that nodes weren't inserted
          -- because of full buckets.
          lookupCheck tree node || not (tree `contains` (node, 0)))

-- | Make sure the bucket sizes end up correct
bucketSizeCheck :: NodeBunch IdType -> IdType -> Bool
bucketSizeCheck = withTree $ \tree _ -> return $ T.fold foldingFunc True tree
  where
    foldingFunc _ False = False
    foldingFunc b _     = length b <= configK defaultConfig

-- | Make sure refreshed Nodes are actually refreshed
refreshCheck :: NodeBunch IdType -> IdType -> Bool
refreshCheck = withTree $ \tree ns -> do
  let node = last ns
      foldingFunc _  False = False
      foldingFunc b _      = node `notElem` b
                             || head b == node
  refreshed <- T.insert tree node (0 :: Timestamp)
  pure (T.fold foldingFunc True refreshed)

-- | Make sure findClosest returns the Node with the closest Ids of all nodes
--   in the tree.
findClosestCheck :: IdType -> NodeBunch IdType -> IdType -> Property
findClosestCheck nid = withTree $ \tree ns -> do
  let contains node = isJust <$> T.lookup tree (nodeId node)
      distanceF = distance nid . nodeId
  contained <- filterM contains ns
  treeClosest <- T.findClosest tree nid $ configK defaultConfig
  let packed = zip contained (map distanceF contained)

  let g node props = counterexample (text node) (prop node):props
                       where prop node' = node' `elem` treeClosest
                             text node' = "Failed to find: " ++ show node'
  let k = configK defaultConfig
  let manualClosest = map fst (take k (sortBy (comparing snd) packed))

  return . conjoin . foldr g [] $ manualClosest

-- | Check that 'T.pickupNotClosest' doesn't return closest nodes.
pickupNotClosestDifferentCheck :: IdType -> NodeBunch IdType -> IdType -> Property
pickupNotClosestDifferentCheck nid = withTree $ \tree _ -> do
    let k = configK defaultConfig
    closest <- T.findClosest tree nid k
    let notClosest = T.pickupRandom tree k closest (mkStdGen 42)
    return . property $ all (`notElem` notClosest) closest

-- | Make sure `toView` represents tree correctly
viewCheck :: NodeBunch IdType -> IdType -> Bool
viewCheck = withTree $ \tree ns -> do
    let originId = T.extractId tree
    let view = map (map fst) (T.toView tree)
    let sorted = map (sort . map (distance originId . nodeId)) view
      -- distance to this node increases from bucket to bucket
    return $  increases (concat sorted)
              -- and view contains all nodes from tree
           && sameElements ns (concat view)
  where
    increases x  = x == sort x
    sameElements = (==) `on` S.fromList

--------------------------------------------------------------------------------

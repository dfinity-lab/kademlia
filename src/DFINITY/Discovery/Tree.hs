--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Tree
-- Description: Implementation of the Node Storage Tree
--
-- "DFINITY.Discovery.Tree" implements the /node storage tree/ used to store
-- and look up the known nodes.
--
-- This module is designed to be used as a qualified import.

--------------------------------------------------------------------------------

module DFINITY.Discovery.Tree
  ( NodeTree (..)
  , NodeTreeElem (..)
  , PingInfo (..)
  , NodesWithPing
  , NodeCache
  , create
  , insert
  , lookup
  , delete
  , handleTimeout
  , pickupRandom
  , findClosest
  , extractId
  , toView
  , toList
  , fold
  ) where

--------------------------------------------------------------------------------

import           Prelude                  hiding (lookup)

import           Control.Arrow            (second)
import           Control.Monad.Random     (evalRand)
import           Data.Function            ((&))
import           GHC.Generics             (Generic)
import           System.Random            (StdGen)
import           System.Random.Shuffle    (shuffleM)

import qualified Data.List                as List

import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map

import           DFINITY.Discovery.Config
                 (KademliaConfig (..), WithConfig, getConfig)
import           DFINITY.Discovery.Types
                 (ByteStruct, Ident, Node (..), Peer, Timestamp,
                 fromByteStruct, sortByDistanceTo, toByteStruct)

--------------------------------------------------------------------------------

-- | FIXME: doc
data NodeTree
  = NodeTree
    { nodeTreeOwnId :: !ByteStruct
    , nodeTreeRoot  :: !NodeTreeElem
    , nodeTreePeers :: !(Map Peer Ident)
    }
  deriving (Generic)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype PingInfo
  = PingInfo
    { pingInfoLastSeen :: Timestamp
      -- ^ FIXME: doc
    }
  deriving (Eq, Generic)

--------------------------------------------------------------------------------

-- | FIXME: doc
data NodeTreeElem
  = -- | FIXME: doc
    Split
    !NodeTreeElem
    -- ^ FIXME: doc
    !NodeTreeElem
    -- ^ FIXME: doc
  | -- | FIXME: doc
    Bucket
    !NodesWithPing
    -- ^ FIXME: doc
    !NodeCache
    -- ^ FIXME: doc
  deriving (Generic)

--------------------------------------------------------------------------------

-- | FIXME: doc
type NodeCache = [Node]

-- | FIXME: doc
type NodesWithPing = [(Node, PingInfo)]

--------------------------------------------------------------------------------

-- FIXME: doc
type NodeTreeFunction a
  = Depth
    -> Validity
    -> Map Peer Ident
    -> NodesWithPing
    -> NodeCache
    -> WithConfig a

-- FIXME: doc
type Depth = Int

-- FIXME: doc
type Validity = Bool

--------------------------------------------------------------------------------

-- | Create a NodeTree corresponding to the id
create :: Ident -> NodeTree
create nid = NodeTree (toByteStruct nid) (Bucket [] []) mempty

--------------------------------------------------------------------------------

-- | Lookup a node within a NodeTree
lookup
  :: NodeTree
  -> Ident
  -> WithConfig (Maybe Node)
lookup tree nid
  = let f :: NodeTreeFunction (Maybe Node)
        f _ _ _ nodes _ = pure $ List.find (idMatches nid) (map fst nodes)
    in applyAt tree nid f


--------------------------------------------------------------------------------

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete
  :: NodeTree
  -> Peer
  -> WithConfig NodeTree
delete tree peer
  = let f :: Ident -> NodeTreeFunction (NodeTreeElem, Map Peer Ident)
        f nid _ _ peers nodes cache
          = let filtered = filter (not . idMatches nid . fst) nodes
            in pure (Bucket filtered cache, Map.delete peer peers)
    in Map.lookup peer (nodeTreePeers tree)
       & maybe (pure tree) (\nid -> modifyAt tree nid (f nid))

--------------------------------------------------------------------------------

-- |
-- Handle a timed out node by incrementing its timeoutCount and deleting it
-- if the count exceeds the limit. Also, return whether it's reasonable to ping
-- the node again.
handleTimeout
  :: Timestamp
  -> NodeTree
  -> Peer
  -> WithConfig (NodeTree, Bool)
handleTimeout currentTime tree peer = do
  case Map.lookup peer (nodeTreePeers tree) of
    Nothing    -> pure (tree, False)
    (Just nid) -> do
      KademliaConfig {..} <- getConfig
      let acceptDiff :: Timestamp
          acceptDiff = fromIntegral configPingLimit
                       * fromIntegral configPingTime
      let f _ _ peers nodes cache = do
            case List.find (idMatches nid . fst) nodes of
              -- FIXME: doc
              Just x@(n, PingInfo lastSeen) -> do
                if ((currentTime - lastSeen) > acceptDiff)
                  then (do -- Delete a node that exceeded the limit.
                           -- Don't contact it again as it is now dead.
                           let nodes' = List.delete x nodes
                           let peers' = Map.delete peer peers
                           pure (Bucket nodes' cache, peers', False))
                  else (do -- Increment the @timeoutCount@.
                           let nodes' = (n, PingInfo lastSeen)
                                        : List.delete x nodes
                           let peers' = peers
                           pure (Bucket nodes' cache, peers', True))
              -- Don't contact an unknown node a second time
              Nothing -> pure (Bucket nodes cache, peers, False)
      modifyApplyAt tree nid f

--------------------------------------------------------------------------------

-- |
-- Refresh the node corresponding to a supplied ID by placing it at the first
-- index of its k-bucket and resetting its @timeoutCount@ and @timestamp@,
-- then return a @('Bucket' … …) :: 'NodeTreeElem' i@.
refresh
  :: Node
  -> Timestamp
  -> NodesWithPing
  -> NodeCache
  -> NodeTreeElem
refresh node currentTimestamp nodes cache
  = let nodes' = case List.find (idMatches (nodeId node) . fst) nodes of
                   Just x@(n, _) -> (n, PingInfo currentTimestamp)
                                    : List.delete x nodes
                   _             -> nodes
    in Bucket nodes' cache

--------------------------------------------------------------------------------

-- |
-- Insert the given 'Node' into the given 'NodeTree'.
insert
  :: NodeTree
  -> Node
  -> Timestamp
  -> WithConfig NodeTree
insert tree node currentTime = do
  let (Node { nodeId = nid, .. }) = node
  k <- configK <$> getConfig
  cacheSize <- configCacheSize <$> getConfig

  -- Check whether a bucket needs splitting FIXME
  let needsSplit :: NodeTreeFunction Bool
      needsSplit depth valid _ nodes _ = do
        let maxDepth = length (toByteStruct nid) - 1

        -- True iff a new node will be inserted.
        let newNodeWillBeInserted
              = node `notElem` map fst nodes

        -- True iff the bucket is full.
        let bucketIsFull
              = length nodes >= k

        -- True iff the bucket may be split.
        --
        -- @georgeee:
        --  I peered at this code for ~30-40 mins.
        --  I clearly don't understand what the reason was to
        --  introduce `depth < 5`.
        --  Maybe some kind of ±1, to not care about a corner case?
        let bucketMayBeSplit
              = ((depth < 5) || valid) && (depth <= maxDepth)

        -- A bucket needs splitting iff:
        -- 1. A new node is about to be inserted.
        -- 2. The bucket is currently full.
        -- 3. The bucket is allowed to be split.
        pure $ and [ newNodeWillBeInserted
                   , bucketIsFull
                   , bucketMayBeSplit
                   ]

  let doInsert :: NodeTreeFunction (NodeTreeElem, Map Peer Ident)
      doInsert _ _ peers nodes cache = do
        pure (if -- Refresh an already existing node
                 | (node `elem` map fst nodes)
                   -> ( refresh node currentTime nodes cache
                      , peers
                      )
                 -- Simply insert the node, if the bucket isn't full
                 | (length nodes < k)
                   -> ( Bucket ((node, PingInfo currentTime) : nodes) cache
                      , Map.insert nodePeer nid peers
                      )
                 -- Move the node to the first spot, if it's already cached
                 | (node `elem` cache)
                   -> ( Bucket nodes (node : List.delete node cache)
                      , peers
                      )
                 -- Cache the node and drop older ones, if necessary
                 | otherwise
                   -> ( Bucket nodes (node : take (cacheSize - 1) cache)
                      , peers
                      ))

  r <- applyAt tree nid needsSplit

  if r
    then (do -- Split the tree before inserting, when it makes sense
             t <- split tree (nodeId node)
             insert t node currentTime)
    else (do -- Insert the node
             modifyAt tree nid doInsert)

--------------------------------------------------------------------------------

-- |
-- Split the k-bucket the specified ID would reside in into two and return
-- a @('Split' … …) :: 'NodeTreeElem' i@ wrapped in a 'NodeTree'.
split :: NodeTree -> Ident -> WithConfig NodeTree
split tree splitId = modifyAt tree splitId g
  where
    g depth _ peers nodes cache = do
      (leftNodes, rightNodes) <- splitBucket depth fst nodes
      (leftCache, rightCache) <- splitBucket depth id cache
      pure ( Split
             (Bucket leftNodes  leftCache)
             (Bucket rightNodes rightCache)
           , peers
           )

    -- Recursivly split the nodes into two buckets
    splitBucket i f
      = \case []     -> pure ([], [])
              (n:ns) -> do let bs = toByteStruct (nodeId (f n))
                           let bit = bs !! i
                           (left, right) <- splitBucket i f ns
                           pure $ if bit
                                  then (left, n : right)
                                  else (n : left, right)

--------------------------------------------------------------------------------

-- | Returns @n@ random nodes from @all 'List.\\' ignoredList@.
pickupRandom
  :: NodeTree
  -> Int
  -> [Node]
  -> StdGen
  -> [Node]
pickupRandom tree n ignoreList randGen
  | (n < 0)
  = error "pickupRandom: assertion failed: n < 0"
  | (n == 0)
  = []
  | otherwise
  = let treeList      = toList tree
        notIgnored    = filter (`notElem` ignoreList) $ map fst treeList
        shuffledNodes = evalRand (shuffleM notIgnored) randGen
    in List.genericTake n shuffledNodes

--------------------------------------------------------------------------------

-- | Find the k closest Nodes to a given Id
findClosest
  :: NodeTree
  -> Ident
  -> Int
  -> WithConfig [Node]
findClosest (NodeTree idStruct treeElem _) nid n = do
  let chooseClosest :: [Node] -> [Node]
      chooseClosest nodes = take n (sortByDistanceTo nodes nid)

  -- FIXME: combine left and right clauses in `go`

  -- This function is partial for the same reason as in 'modifyAt'.
  let go :: ByteStruct
         -> ByteStruct
         -> NodeTreeElem
         -> WithConfig [Node]
      go is ts el = do
        case (is, ts, el) of
          -- Take the @n@ closest nodes.
          (_, _, Bucket nodePairs _) -> do
            let nodes = map fst nodePairs
            pure $ if length nodes <= n
                   then nodes
                   else chooseClosest nodes
          -- Take the closest nodes from the left child first and if those
          -- aren't enough, take the rest from the right.
          (_ : irest, False : trest, Split left right) -> do
            result <- go irest trest left
            if length result == n
              then pure result
              else (result ++) <$> go irest trest right
          -- Take the closest nodes from the right child first and if those
          -- aren't enough, take the rest from the left.
          (_ : irest, True  : trest, Split left right) -> do
            result <- go irest trest right
            if length result == n
              then pure result
              else (result ++) <$> go irest trest left
          -- Something has gone terribly wrong.
          _ -> do
            error "Fundamental error in @go@ function in 'findClosest'"

  let targetStruct = toByteStruct nid
  chooseClosest <$> go idStruct targetStruct treeElem

--------------------------------------------------------------------------------

-- | Extract original ID from NodeTree
extractId :: NodeTree -> Ident
extractId (NodeTree nid _ _) = fromByteStruct nid

--------------------------------------------------------------------------------

-- | Helper function used for KBucket manipulation
idMatches :: Ident -> Node -> Bool
idMatches nid node = nid == nodeId node

--------------------------------------------------------------------------------

-- | Turn the NodeTree into a list of buckets, ordered by distance to origin node
toView :: NodeTree -> [[(Node, Timestamp)]]
toView (NodeTree bs treeElems _) = go bs treeElems []
  where
    -- If the bit is 0, go left, then right
    go (False : is) (Split left right) = go is left . go is right
    -- Otherwise go right first
    go (True  : is) (Split left right) = go is right . go is left
    go _            (Split _    _    ) = error "toView: unexpected Split"
    go _            (Bucket b _)       = (map (second pingInfoLastSeen) b :)

--------------------------------------------------------------------------------

-- | Turn the NodeTree into a list of nodes
toList :: NodeTree -> [(Node, Timestamp)]
toList = concat . toView

--------------------------------------------------------------------------------

-- | Fold over the buckets
fold :: ([Node] -> a -> a) -> a -> NodeTree -> a
fold f start (NodeTree _ treeElems _) = go start treeElems
    where go a (Split left right) = let a' = go a left in go a' right
          go a (Bucket nodes _)   = f (map fst nodes) a

--------------------------------------------------------------------------------

-- FIXME: try to figure out what this comment is saying and reword it
-- There are three similar functions, its go down by tree using passed NodeId.
-- When its reach leaf with bucket its apply, modify or apply and modify stored bucket.

--------------------------------------------------------------------------------

-- | Modify the position in the tree where the supplied ID would be.
modifyAt
  :: NodeTree
  -> Ident
  -> NodeTreeFunction (NodeTreeElem, Map Peer Ident)
  -> WithConfig NodeTree
modifyAt tree nid f
  = let md1 (x, y) = (x, y, ())
    in fst <$> modifyApplyAt tree nid (\a b c d e -> md1 <$> f a b c d e)

--------------------------------------------------------------------------------

-- |
-- Apply a function to the bucket the supplied ID would be located in.
applyAt
  :: forall a
  .  NodeTree
  -> Ident
  -> NodeTreeFunction a
  -> WithConfig a
applyAt (NodeTree idStruct treeElem peers) nid f = do
  -- FIXME: combine left and right clauses in `go`

  -- This function is partial for the same reason as in modifyAt
  let go :: ByteStruct -> ByteStruct -> Depth -> Validity -> NodeTreeElem
         -> WithConfig a
      go is ts depth valid el = do
        case (is, ts, el) of
          -- Apply the function
          (_, _, Bucket nodes cache) -> do
            f depth valid peers nodes cache
          -- If the bit is a 0, go left
          (i : irest, False : trest, Split left _) -> do
            go irest trest (depth + 1) (valid && not i) left
          -- Otherwise, continue to the right
          (i : irest, True  : trest, Split _ right) -> do
            go irest trest (depth + 1) (valid &&     i) right
          -- Something has gone terribly wrong.
          _ -> do
            error "Fundamental error in @go@ function in 'applyAt'"

  let targetStruct = toByteStruct nid
  go idStruct targetStruct 0 True treeElem

--------------------------------------------------------------------------------

-- | Modify and apply a function at the position in the tree where the
-- supplied id would be
modifyApplyAt
  :: forall a
  .  NodeTree
  -> Ident
  -> NodeTreeFunction (NodeTreeElem, Map Peer Ident, a)
  -> WithConfig (NodeTree, a)
modifyApplyAt (NodeTree idStruct treeElem peers) nid f = do
  -- FIXME: combine left and right clauses in `go`

  -- This function is partial, but we know that there will alwasys be a
  -- bucket at the end. Therefore, we don't have to check for empty
  -- ByteStructs.
  let go :: ByteStruct -> ByteStruct -> Depth -> Validity -> NodeTreeElem
         -> WithConfig (NodeTreeElem, Map Peer Ident, a)
      go is ts depth valid el = do
        case (is, ts, el) of
          -- Apply the function to the position of the bucket
          (_, _, Bucket nodes cache) -> do
            f depth valid peers nodes cache
          -- If the bit is a 0, go left
          (i : irest, False : trest, Split left right) -> do
            (new, ms, val) <- go irest trest (depth + 1) (valid && not i) left
            pure (Split new right, ms, val)
          -- Otherwise, continue to the right
          (i : irest, True : trest, Split left right) -> do
            (new, ms, val) <- go irest trest (depth + 1) (valid &&     i) right
            pure (Split left new, ms, val)
          -- Something has gone terribly wrong.
          _ -> do
            error "Fundamental error in @go@ function in 'modifyApplyAt'"

  let targetStruct = toByteStruct nid
  (newElems, mpeers, val) <- go idStruct targetStruct 0 True treeElem
  pure (NodeTree idStruct newElems mpeers, val)

--------------------------------------------------------------------------------

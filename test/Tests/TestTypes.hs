--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

--------------------------------------------------------------------------------

-- |
-- Module:      Tests.TestTypes
-- Description: Types and Generators needed for general testing

--------------------------------------------------------------------------------

module Tests.TestTypes
  ( NodeBunch (..)
  , IdBunch   (..)
  ) where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Char8      as C
import           Data.Function              (on)
import           Data.List                  (nubBy)
import           Data.Word                  (Word16)
import           Network.Socket             (PortNumber)

import           Test.QuickCheck
                 (Arbitrary (..), oneof, suchThat, vectorOf)
import           Test.QuickCheck.Instances  ()

import           Data.IP                    (IP (IPv4, IPv6))
import qualified Data.IP                    as IP

import           DFINITY.Discovery.Instance (BanState (..))
import           DFINITY.Discovery.Types
                 (Command (..), Ident (..), Node (..), Peer (..), Signal (..))

--------------------------------------------------------------------------------

instance Arbitrary Ident where
  arbitrary = do
    str <- vectorOf 5 arbitrary
    pure $ Ident $ C.pack str

-- instance Arbitrary Value where
--   arbitrary = do
--     str <- vectorOf 10 arbitrary
--     pure $ Value $ C.pack str

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (arbitrary @Word16)

instance Arbitrary Peer where
  arbitrary = do
    host <- oneof
            [ (IPv4 . IP.fromHostAddress  <$> arbitrary)
            , (IPv6 . IP.fromHostAddress6 <$> arbitrary)
            ]
    port <- arbitrary
    pure (Peer host port)

instance Arbitrary Command where
  arbitrary = oneof
              [ pure PING
              , pure PONG
              , FIND_NODE <$> arbitrary
              , RETURN_NODES 1 <$> arbitrary <*> vectorOf 30 arbitrary
              ]

instance Arbitrary Signal where
  arbitrary = Signal <$> arbitrary <*> arbitrary

instance Arbitrary Node where
  arbitrary = Node <$> arbitrary <*> arbitrary

-- | This enables me to specifiy a new Arbitrary instance
newtype NodeBunch
  = NodeBunch { nodes :: [Node] }
  deriving (Show)

-- | Make sure all Ids are unique
instance Arbitrary NodeBunch where
  arbitrary
    = NodeBunch <$> vectorOf 20 arbitrary `suchThat` individualIds
    where
      individualIds = individual ((==) `on` nodeId)

individual :: (a -> a -> Bool) -> [a] -> Bool
individual eq s = length s == length (nubBy eq s)

-- | This is needed for the Implementation tests
newtype IdBunch
  = IdBunch { getIds :: [Ident] }
  deriving (Show)

instance Arbitrary IdBunch where
  arbitrary = IdBunch <$> vectorOf 40 arbitrary `suchThat` individual (==)

instance Arbitrary BanState where
  arbitrary = oneof
              [ pure BanForever
              , pure NoBan
              , BanTill <$> arbitrary
              ]

--------------------------------------------------------------------------------

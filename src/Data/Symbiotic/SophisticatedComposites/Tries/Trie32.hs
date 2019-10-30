{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.SophisticatedComposites.Tries.Trie32 where

import Data.Symbiotic.SophisticatedComposites.Tries.Trie32.Orphans ()

import qualified Data.Trie.Map as MT
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson.Types (typeMismatch, Parser, Value)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost', arbitraryMaybe)


newtype Trie32 k a = Trie32 {getTrie32 :: MT.MapTrie k a}
  deriving (Generic, Show, Eq, Ord, Serialize)

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (Trie32 k a) where
  arbitrary = Trie32 <$> trie
    where
      trie = MT.MapTrie <$> step
      step =
        let elems = (,) <$> arbitrary <*> children
        in  MT.MapStep . Map.fromList <$> atMost' ((2 :: Int) ^ (32 :: Int)) elems
      children = MT.MapChildren <$> arbitrary <*> arbitraryMaybe trie

instance (ToJSON k, ToJSON a) => ToJSON (Trie32 k a) where
  toJSON (Trie32 xs) = trie xs
    where
      trie (MT.MapTrie ys) = step ys
      step (MT.MapStep ys) = toJSON (Map.toList (children <$> ys))
      children (MT.MapChildren my ys) = toJSON [toJSON <$> my, trie <$> ys]

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Trie32 k a) where
  parseJSON json = Trie32 <$> trie json
    where
      trie j = MT.MapTrie <$> step j
      step j = do
        xs <- parseJSON j
        let xs' = Map.fromList xs
        xs'' <- traverse children xs'
        pure (MT.MapStep xs'')
      children j = do
        xs <- parseJSON j :: Parser (V.Vector (Maybe Value))
        if length xs /= 2
          then fail'
          else do
            my <- sequenceA (parseJSON <$> (xs V.! 0))
            ys <- sequenceA (trie <$> (xs V.! 1))
            pure (MT.MapChildren my ys)

      fail' = typeMismatch "Trie32" json

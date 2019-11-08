{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.Map32 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (getVector32, makeVector32)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Serialize (Serialize (..))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype Map32 k a = Map32 {getMap32 :: Map k a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (Map32 k a) where
  arbitrary = Map32 . Map.fromList <$> atMost ((2 :: Int) ^ (10 :: Int))

instance (ToJSON k, ToJSON a) => ToJSON (Map32 k a) where
  toJSON (Map32 xs) = case makeVector32 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector32 can't be made from Map32"
    Just ys -> toJSON ys

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Map32 k a) where
  parseJSON json = do
    xs <- getVector32 <$> parseJSON json
    pure (Map32 (Map.fromList (V.toList xs)))

instance (Serialize k, Serialize a, Ord k) => Serialize (Map32 k a) where
  put (Map32 xs) = case makeVector32 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector32 can't be made from Map32"
    Just ys -> put ys
  get = do
    xs <- getVector32 <$> get
    pure (Map32 (Map.fromList (V.toList xs)))

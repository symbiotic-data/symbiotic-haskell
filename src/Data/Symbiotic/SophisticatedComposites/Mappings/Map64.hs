{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.Map64 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (getVector64, makeVector64)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Serialize (Serialize (..))
import qualified Data.Vector as V
import GHC.Generics (Generic)


newtype Map64 k a = Map64 {getMap64 :: Map k a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance (ToJSON k, ToJSON a) => ToJSON (Map64 k a) where
  toJSON (Map64 xs) = case makeVector64 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector64 can't be made from Map64"
    Just ys -> toJSON ys

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Map64 k a) where
  parseJSON json = do
    xs <- getVector64 <$> parseJSON json
    pure (Map64 (Map.fromList (V.toList xs)))

instance (Serialize k, Serialize a, Ord k) => Serialize (Map64 k a) where
  put (Map64 xs) = case makeVector64 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector64 can't be made from Map64"
    Just ys -> put ys
  get = do
    xs <- getVector64 <$> get
    pure (Map64 (Map.fromList (V.toList xs)))

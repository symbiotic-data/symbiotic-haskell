{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.Map16 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (getVector16, makeVector16)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype Map16 k a = Map16 {getMap16 :: Map k a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance (ToJSON k, ToJSON a) => ToJSON (Map16 k a) where
  toJSON (Map16 xs) = case makeVector16 (Map.toList xs) of
    Nothing -> error "Vector16 can't be made from Map16"
    Just ys -> toJSON ys

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Map16 k a) where
  parseJSON json = do
    xs <- getVector16 <$> parseJSON json
    pure (Map16 (Map.fromList xs))

instance (Serialize k, Serialize a, Ord k) => Serialize (Map16 k a) where
  put (Map16 xs) = case makeVector16 (Map.toList xs) of
    Nothing -> error "Vector16 can't be made from Map16"
    Just ys -> put ys
  get = do
    xs <- getVector16 <$> get
    pure (Map16 (Map.fromList xs))

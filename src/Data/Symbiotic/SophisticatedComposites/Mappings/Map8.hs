{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.Map8 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (getVector8, makeVector8)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Serialize (Serialize (..))
import qualified Data.Vector as V
import GHC.Generics (Generic)


newtype Map8 k a = Map8 {getMap8 :: Map k a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance (ToJSON k, ToJSON a) => ToJSON (Map8 k a) where
  toJSON (Map8 xs) = case makeVector8 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector8 can't be made from Map8"
    Just ys -> toJSON ys

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Map8 k a) where
  parseJSON json = do
    xs <- getVector8 <$> parseJSON json
    pure (Map8 (Map.fromList (V.toList xs)))

instance (Serialize k, Serialize a, Ord k) => Serialize (Map8 k a) where
  put (Map8 xs) = case makeVector8 (V.fromList (Map.toList xs)) of
    Nothing -> error "Vector8 can't be made from Map8"
    Just ys -> put ys
  get = do
    xs <- getVector8 <$> get
    pure (Map8 (Map.fromList (V.toList xs)))

{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector8
  ( Vector8, getVector8, makeVector8
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array), fromJSON, Result (Error, Success))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord8)
import Data.Serialize.Put (putWord8)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void, replicateM)


newtype Vector8 a = Vector8 {getVector8 :: V.Vector a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable)


makeVector8 :: V.Vector a -> Maybe (Vector8 a)
makeVector8 x
  | V.length x < hi = Just (Vector8 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (8 :: Int)

instance (ToJSON a) => ToJSON (Vector8 a) where
  toJSON (Vector8 x) = Array (V.map toJSON x)

instance (FromJSON a) => FromJSON (Vector8 a) where
  parseJSON json = case json of
    Array x -> case makeVector8 x of
      Nothing -> fail'
      Just y -> traverse parseJSON y
    _ -> fail'
    where
      fail' = typeMismatch "Vector8" json

instance (Serialize a) => Serialize (Vector8 a) where
  put (Vector8 x) = do
    putWord8 (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord8
    Vector8 <$> V.replicateM (fromIntegral l) get

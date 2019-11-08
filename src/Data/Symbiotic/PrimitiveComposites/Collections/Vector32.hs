{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector32
  ( Vector32, getVector32, makeVector32
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord32be)
import Data.Serialize.Put (putWord32be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype Vector32 a = Vector32 {getVector32 :: V.Vector a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Vector32 a) where
  arbitrary = Vector32 . V.fromList <$> atMost ((2 :: Int) ^ (10 :: Int))


makeVector32 :: V.Vector a -> Maybe (Vector32 a)
makeVector32 x
  | V.length x < hi = Just (Vector32 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (32 :: Int)

instance (ToJSON a) => ToJSON (Vector32 a) where
  toJSON (Vector32 x) = Array (V.map toJSON x)

instance (FromJSON a) => FromJSON (Vector32 a) where
  parseJSON json = case json of
    Array x -> case makeVector32 x of
      Nothing -> fail'
      Just y -> traverse parseJSON y
    _ -> fail'
    where
      fail' = typeMismatch "Vector32" json

instance (Serialize a) => Serialize (Vector32 a) where
  put (Vector32 x) = do
    putWord32be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord32be
    Vector32 <$> V.replicateM (fromIntegral l) get

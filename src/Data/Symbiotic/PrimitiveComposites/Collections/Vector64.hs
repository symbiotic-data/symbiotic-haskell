{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector64
  ( Vector64, getVector64, makeVector64
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord64be)
import Data.Serialize.Put (putWord64be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype Vector64 a = Vector64 {getVector64 :: V.Vector a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Vector64 a) where
  arbitrary = Vector64 . V.fromList <$> atMost ((2 :: Int) ^ (10 :: Int))


makeVector64 :: V.Vector a -> Maybe (Vector64 a)
makeVector64 x
  | V.length x < hi = Just (Vector64 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (62 :: Int)

instance (ToJSON a) => ToJSON (Vector64 a) where
  toJSON (Vector64 x) = Array (V.map toJSON x)

instance (FromJSON a) => FromJSON (Vector64 a) where
  parseJSON json = case json of
    Array x -> case makeVector64 x of
      Nothing -> fail'
      Just y -> traverse parseJSON y
    _ -> fail'
    where
      fail' = typeMismatch "Vector64" json

instance (Serialize a) => Serialize (Vector64 a) where
  put (Vector64 x) = do
    putWord64be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord64be
    Vector64 <$> V.replicateM (fromIntegral l) get

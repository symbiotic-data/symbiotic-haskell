{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector16
  ( Vector16, getVector16, makeVector16
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord16be)
import Data.Serialize.Put (putWord16be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype Vector16 a = Vector16 {getVector16 :: V.Vector a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable)

instance Arbitrary a => Arbitrary (Vector16 a) where
  arbitrary = Vector16 . V.fromList <$> atMost ((2 :: Int) ^ (10 :: Int))


makeVector16 :: V.Vector a -> Maybe (Vector16 a)
makeVector16 x
  | V.length x < hi = Just (Vector16 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (16 :: Int)

instance (ToJSON a) => ToJSON (Vector16 a) where
  toJSON (Vector16 x) = Array (V.map toJSON x)

instance (FromJSON a) => FromJSON (Vector16 a) where
  parseJSON json = case json of
    Array x -> case makeVector16 x of
      Nothing -> fail'
      Just y -> traverse parseJSON y
    _ -> fail'
    where
      fail' = typeMismatch "Vector16" json

instance (Serialize a) => Serialize (Vector16 a) where
  put (Vector16 x) = do
    putWord16be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord16be
    Vector16 <$> V.replicateM (fromIntegral l) get

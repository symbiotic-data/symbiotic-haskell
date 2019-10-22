{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector32
  ( Vector32, getVector32, makeVector32
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array), fromJSON, Result (Error, Success))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Unfolder (Unfolder)
import Data.Unfoldable (Unfoldable (..), unfoldr, fromList)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord32be)
import Data.Serialize.Put (putWord32be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void, replicateM)


newtype Vector32 f a = Vector32 {getVector32 :: f a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable, Unfolder)

instance Unfoldable f => Unfoldable (Vector32 f) where
  unfold x = Vector32 <$> unfold x


makeVector32 :: Foldable f => f a -> Maybe (Vector32 f a)
makeVector32 x
  | length x < hi = Just (Vector32 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (32 :: Int)

instance (ToJSON a, Foldable f) => ToJSON (Vector32 f a) where
  toJSON (Vector32 x) = Array (foldr (V.cons . toJSON) V.empty x)

instance (FromJSON a, Unfoldable f) => FromJSON (Vector32 f a) where
  parseJSON json = case json of
    Array x -> case unfoldr go x of
      Nothing -> fail'
      Just y -> pure y
    _ -> fail'
    where
      fail' = typeMismatch "Vector32" json
      go v = case v V.!? 0 of
        Nothing -> Nothing
        Just head' -> case fromJSON head' of
          Error _ -> Nothing
          Success y -> Just (y, V.tail v)

instance (Serialize a, Traversable f, Unfoldable f) => Serialize (Vector32 f a) where
  put (Vector32 x) = do
    putWord32be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord32be
    xs <- replicateM (fromIntegral l) get
    case fromList xs of
      Nothing -> fail "Vector32"
      Just y -> pure (Vector32 y)

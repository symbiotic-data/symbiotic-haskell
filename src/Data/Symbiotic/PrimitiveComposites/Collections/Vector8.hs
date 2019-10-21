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
import Data.Unfolder (Unfolder)
import Data.Unfoldable (Unfoldable (..), unfoldr, fromList)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord8)
import Data.Serialize.Put (putWord8)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void, replicateM)


newtype Vector8 f a = Vector8 {getVector8 :: f a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable, Unfolder)

instance Unfoldable f => Unfoldable (Vector8 f) where
  unfold x = Vector8 <$> unfold x


makeVector8 :: Foldable f => f a -> Maybe (Vector8 f a)
makeVector8 x
  | length x < hi = Just (Vector8 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (8 :: Int)

instance (ToJSON a, Foldable f) => ToJSON (Vector8 f a) where
  toJSON (Vector8 x) = Array (foldr (V.cons . toJSON) V.empty x)

instance (FromJSON a, Unfoldable f) => FromJSON (Vector8 f a) where
  parseJSON json = case json of
    Array x -> case unfoldr go x of
      Nothing -> fail'
      Just y -> pure y
    _ -> fail'
    where
      fail' = typeMismatch "Vector8" json
      go v = case v V.!? 0 of
        Nothing -> Nothing
        Just head' -> case fromJSON head' of
          Error _ -> Nothing
          Success y -> Just (y, V.tail v)

instance (Serialize a, Traversable f, Unfoldable f) => Serialize (Vector8 f a) where
  put (Vector8 x) = do
    putWord8 (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord8
    xs <- replicateM (fromIntegral l) get
    case fromList xs of
      Nothing -> fail "Vector8"
      Just y -> pure (Vector8 y)

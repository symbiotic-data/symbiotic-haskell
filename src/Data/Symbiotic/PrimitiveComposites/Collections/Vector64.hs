{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector64
  ( Vector64, getVector64, makeVector64
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array), fromJSON, Result (Error, Success))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Unfolder (Unfolder)
import Data.Unfoldable (Unfoldable (..), unfoldr, fromList)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord64be)
import Data.Serialize.Put (putWord64be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void, replicateM)


newtype Vector64 f a = Vector64 {getVector64 :: f a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable, Unfolder)

instance Unfoldable f => Unfoldable (Vector64 f) where
  unfold x = Vector64 <$> unfold x


makeVector64 :: Foldable f => f a -> Maybe (Vector64 f a)
makeVector64 x
  | length x < hi = Just (Vector64 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (64 :: Int)

instance (ToJSON a, Foldable f) => ToJSON (Vector64 f a) where
  toJSON (Vector64 x) = Array (foldr (V.cons . toJSON) V.empty x)

instance (FromJSON a, Unfoldable f) => FromJSON (Vector64 f a) where
  parseJSON json = case json of
    Array x -> case unfoldr go x of
      Nothing -> fail'
      Just y -> pure y
    _ -> fail'
    where
      fail' = typeMismatch "Vector64" json
      go v = case v V.!? 0 of
        Nothing -> Nothing
        Just head' -> case fromJSON head' of
          Error _ -> Nothing
          Success y -> Just (y, V.tail v)

instance (Serialize a, Traversable f, Unfoldable f) => Serialize (Vector64 f a) where
  put (Vector64 x) = do
    putWord64be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord64be
    xs <- replicateM (fromIntegral l) get
    case fromList xs of
      Nothing -> fail "Vector64"
      Just y -> pure (Vector64 y)

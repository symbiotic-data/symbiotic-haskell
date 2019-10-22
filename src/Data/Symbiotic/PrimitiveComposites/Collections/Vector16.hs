{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Collections.Vector16
  ( Vector16, getVector16, makeVector16
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Array), fromJSON, Result (Error, Success))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Data.Unfolder (Unfolder)
import Data.Unfoldable (Unfoldable (..), unfoldr, fromList)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord16be)
import Data.Serialize.Put (putWord16be)
import Data.Traversable (Traversable (traverse))
import Control.Applicative (Alternative)
import Control.Monad (void, replicateM)


newtype Vector16 f a = Vector16 {getVector16 :: f a}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, Functor, Applicative, Alternative, Monad, Foldable, Traversable, Unfolder)

instance Unfoldable f => Unfoldable (Vector16 f) where
  unfold x = Vector16 <$> unfold x


makeVector16 :: Foldable f => f a -> Maybe (Vector16 f a)
makeVector16 x
  | length x < hi = Just (Vector16 x)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (16 :: Int)

instance (ToJSON a, Foldable f) => ToJSON (Vector16 f a) where
  toJSON (Vector16 x) = Array (foldr (V.cons . toJSON) V.empty x)

instance (FromJSON a, Unfoldable f) => FromJSON (Vector16 f a) where
  parseJSON json = case json of
    Array x -> case unfoldr go x of
      Nothing -> fail'
      Just y -> pure y
    _ -> fail'
    where
      fail' = typeMismatch "Vector16" json
      go v = case v V.!? 0 of
        Nothing -> Nothing
        Just head' -> case fromJSON head' of
          Error _ -> Nothing
          Success y -> Just (y, V.tail v)

instance (Serialize a, Traversable f, Unfoldable f) => Serialize (Vector16 f a) where
  put (Vector16 x) = do
    putWord16be (fromIntegral (length x))
    void (traverse put x)
  get = do
    l <- getWord16be
    xs <- replicateM (fromIntegral l) get
    case fromList xs of
      Nothing -> fail "Vector16"
      Just y -> pure (Vector16 y)

{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String32 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (makeVector32, getVector32)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype String32 = String32 {getString32 :: T.Text}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Hashable)

instance Arbitrary String32 where
  arbitrary = String32 . T.pack <$> atMost ((2 :: Int) ^ (32 :: Int))

makeString32 :: T.Text -> Maybe String32
makeString32 t
  | T.length t < hi = Just (String32 t)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (32 :: Int)

instance IsString String32 where
  fromString s = case makeString32 (T.pack s) of
    Nothing -> error "Vector32 can't be made from String"
    Just v -> v

instance Serialize String32 where
  put (String32 t) = case makeVector32 (V.fromList (T.unpack t)) of
    Nothing -> error "Vector32 can't be made from String32"
    Just x -> put x
  get = do
    xs <- getVector32 <$> get
    pure (String32 (T.pack (V.toList xs)))

{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String8 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (makeVector8, getVector8)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype String8 = String8 {getString8 :: T.Text}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Hashable)

instance Arbitrary String8 where
  arbitrary = String8 . T.pack <$> atMost ((2 :: Int) ^ (8 :: Int))

makeString8 :: T.Text -> Maybe String8
makeString8 t
  | T.length t < hi = Just (String8 t)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (8 :: Int)

instance IsString String8 where
  fromString s = case makeString8 (T.pack s) of
    Nothing -> error "String8 can't be made from String"
    Just v -> v

instance Serialize String8 where
  put (String8 t) = case makeVector8 (V.fromList (T.unpack t)) of
    Nothing -> error "Vector8 can't be made from String8"
    Just x -> put x
  get = do
    xs <- getVector8 <$> get
    pure (String8 (T.pack (V.toList xs)))

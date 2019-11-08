{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String16 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (makeVector16, getVector16)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype String16 = String16 {getString16 :: T.Text}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Hashable)

instance Arbitrary String16 where
  arbitrary = String16 . T.pack <$> atMost ((2 :: Int) ^ (8 :: Int))

makeString16 :: T.Text -> Maybe String16
makeString16 t
  | T.length t < hi = Just (String16 t)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (16 :: Int)

instance IsString String16 where
  fromString s = case makeString16 (T.pack s) of
    Nothing -> error "Vector16 can't be made from String"
    Just v -> v

instance Serialize String16 where
  put (String16 t) = case makeVector16 (V.fromList (T.unpack t)) of
    Nothing -> error "Vector16 can't be made from String16"
    Just x -> put x
  get = do
    xs <- getVector16 <$> get
    pure (String16 (T.pack (V.toList xs)))

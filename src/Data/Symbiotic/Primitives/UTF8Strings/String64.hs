{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String64 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (makeVector64, getVector64)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype String64 = String64 {getString64 :: T.Text}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Hashable)

instance Arbitrary String64 where
  arbitrary = String64 . T.pack <$> atMost ((2 :: Int) ^ (62 :: Int))

makeString64 :: T.Text -> Maybe String64
makeString64 t
  | T.length t < hi = Just (String64 t)
  | otherwise = Nothing
  where
    hi :: Int
    hi = (2 :: Int) ^ (62 :: Int)

instance IsString String64 where
  fromString s = case makeString64 (T.pack s) of
    Nothing -> error "Vector64 can't be made from String"
    Just v -> v

instance Serialize String64 where
  put (String64 t) = case makeVector64 (V.fromList (T.unpack t)) of
    Nothing -> error "Vector64 can't be made from String64"
    Just x -> put x
  get = do
    xs <- getVector64 <$> get
    pure (String64 (T.pack (V.toList xs)))

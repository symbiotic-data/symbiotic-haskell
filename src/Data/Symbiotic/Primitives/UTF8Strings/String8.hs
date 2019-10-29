{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String8 where

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord8, putByteString)
import Data.Serialize.Get (getWord8, getByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
    Nothing -> error "Vector8 can't be made from String"
    Just v -> v

instance Serialize String8 where
  put (String8 t) = do
    putWord8 (fromIntegral (T.length t))
    putByteString (T.encodeUtf8 t)
  get = do
    l <- getWord8
    xs <- getByteString (fromIntegral l)
    pure (String8 (T.decodeUtf8 xs))

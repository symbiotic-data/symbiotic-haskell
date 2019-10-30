{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  , RankNTypes
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap8 where

import Data.Symbiotic.Primitives.UTF8Strings.String8 (String8)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (getVector8, makeVector8)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, Value (Object), Result (..))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Serialize (Serialize (..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype StringMap8 a = StringMap8 {getStringMap8 :: HM.HashMap String8 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (StringMap8 a) where
  arbitrary = StringMap8 . HM.fromList <$> atMost ((2 :: Int) ^ (8 :: Int))

instance ToJSON a => ToJSON (StringMap8 a) where
  toJSON (StringMap8 xs) = Object (unsafeCoerce (HM.map toJSON xs))

instance FromJSON a => FromJSON (StringMap8 a) where
  parseJSON :: forall a. FromJSON a => Value -> Parser (StringMap8 a)
  parseJSON json = case json of
    Object o -> do
      xs <- traverse parseJSON o :: Parser (HM.HashMap T.Text a)
      pure (StringMap8 (unsafeCoerce xs))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap8" json

instance Serialize a => Serialize (StringMap8 a) where
  put (StringMap8 x) = case makeVector8 (V.fromList (HM.toList x)) of
    Nothing -> error "Serialize - can't make Vector8 out of StringMap8"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap8 (HM.fromList (V.toList (getVector8 x))))

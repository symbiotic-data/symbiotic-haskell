{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  , RankNTypes
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap32 where

import Data.Symbiotic.Primitives.UTF8Strings.String32 (String32 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (getVector32, makeVector32)

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


newtype StringMap32 a = StringMap32 {getStringMap32 :: HM.HashMap String32 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (StringMap32 a) where
  arbitrary = StringMap32 . HM.fromList <$> atMost ((2 :: Int) ^ (32 :: Int))

instance ToJSON a => ToJSON (StringMap32 a) where
  toJSON (StringMap32 xs) = Object (unsafeCoerce (HM.map toJSON xs))

instance FromJSON a => FromJSON (StringMap32 a) where
  parseJSON :: forall a. FromJSON a => Value -> Parser (StringMap32 a)
  parseJSON json = case json of
    Object o -> do
      xs <- traverse parseJSON o :: Parser (HM.HashMap T.Text a)
      pure (StringMap32 (unsafeCoerce xs))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap32" json

instance Serialize a => Serialize (StringMap32 a) where
  put (StringMap32 x) = case makeVector32 (V.fromList (HM.toList x)) of
    Nothing -> error "Serialize - can't make Vector32 out of StringMap32"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap32 (HM.fromList (V.toList (getVector32 x))))

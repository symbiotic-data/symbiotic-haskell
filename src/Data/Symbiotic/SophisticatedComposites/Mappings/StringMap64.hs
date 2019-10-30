{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  , RankNTypes
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap64 where

import Data.Symbiotic.Primitives.UTF8Strings.String64 (String64 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (getVector64, makeVector64)

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


newtype StringMap64 a = StringMap64 {getStringMap64 :: HM.HashMap String64 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (StringMap64 a) where
  arbitrary = StringMap64 . HM.fromList <$> atMost ((2 :: Int) ^ (64 :: Int))

instance ToJSON a => ToJSON (StringMap64 a) where
  toJSON (StringMap64 xs) = Object (unsafeCoerce (HM.map toJSON xs))

instance FromJSON a => FromJSON (StringMap64 a) where
  parseJSON :: forall a. FromJSON a => Value -> Parser (StringMap64 a)
  parseJSON json = case json of
    Object o -> do
      xs <- traverse parseJSON o :: Parser (HM.HashMap T.Text a)
      pure (StringMap64 (unsafeCoerce xs))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap64" json

instance Serialize a => Serialize (StringMap64 a) where
  put (StringMap64 x) = case makeVector64 (V.fromList (HM.toList x)) of
    Nothing -> error "Serialize - can't make Vector64 out of StringMap64"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap64 (HM.fromList (V.toList (getVector64 x))))

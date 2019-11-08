{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap16 where

import Data.Symbiotic.Primitives.UTF8Strings.String16 (String16 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (getVector16, makeVector16)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Serialize (Serialize (..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost)


newtype StringMap16 a = StringMap16 {getStringMap16 :: HM.HashMap String16 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (StringMap16 a) where
  arbitrary = StringMap16 . HM.fromList <$> atMost ((2 :: Int) ^ (10 :: Int))

instance ToJSON a => ToJSON (StringMap16 a) where
  toJSON (StringMap16 xs) = Object (unsafeCoerce (HM.map toJSON xs))

instance FromJSON a => FromJSON (StringMap16 a) where
  parseJSON :: Value -> Parser (StringMap16 a)
  parseJSON json = case json of
    Object o -> do
      xs <- traverse parseJSON o :: Parser (HM.HashMap T.Text a)
      pure (StringMap16 (unsafeCoerce xs))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap16" json

instance Serialize a => Serialize (StringMap16 a) where
  put (StringMap16 x) = case makeVector16 (V.fromList (HM.toList x)) of
    Nothing -> error "Serialize - can't make Vector16 out of StringMap16"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap16 (HM.fromList (V.toList (getVector16 x))))

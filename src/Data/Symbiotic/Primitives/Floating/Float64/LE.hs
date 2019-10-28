{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Float64.LE where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.IEEE754 (getFloat64le, putFloat64le)
import Test.QuickCheck (Arbitrary)

newtype Float64LE = Float64LE {getFloat64LE :: Double}
  deriving (Fractional, Floating, Enum, Real, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Float64LE where
  get = Float64LE <$> getFloat64le
  put (Float64LE x) = putFloat64le x

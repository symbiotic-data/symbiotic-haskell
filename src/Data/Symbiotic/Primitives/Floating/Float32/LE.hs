{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Float32.LE where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.IEEE754 (getFloat32le, putFloat32le)
import Test.QuickCheck (Arbitrary)

newtype Float32LE = Float32LE {getFloat32LE :: Float}
  deriving (Fractional, Floating, Enum, Real, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Float32LE where
  get = Float32LE <$> getFloat32le
  put (Float32LE x) = putFloat32le x

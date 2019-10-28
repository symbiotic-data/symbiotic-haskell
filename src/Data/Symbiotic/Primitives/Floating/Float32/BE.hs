{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Float32.BE where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.IEEE754 (getFloat32be, putFloat32be)
import Test.QuickCheck (Arbitrary)

newtype Float32BE = Float32BE {getFloat32BE :: Float}
  deriving (Fractional, Floating, Enum, Real, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Float32BE where
  get = Float32BE <$> getFloat32be
  put (Float32BE x) = putFloat32be x

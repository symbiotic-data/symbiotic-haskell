{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Float64.BE where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.IEEE754 (getFloat64be, putFloat64be)
import Test.QuickCheck (Arbitrary)

newtype Float64BE = Float64BE {getFloat64BE :: Double}
  deriving (Fractional, Floating, Enum, Real, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Float64BE where
  get = Float64BE <$> getFloat64be
  put (Float64BE x) = putFloat64be x

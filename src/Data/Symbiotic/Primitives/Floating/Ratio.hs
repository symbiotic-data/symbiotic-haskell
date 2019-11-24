{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Ratio where

import qualified Data.Ratio as R
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Serialize (Serialize)
import Test.QuickCheck (Arbitrary)


newtype Ratio a = Ratio (R.Ratio a)
  deriving (Arbitrary, Eq, Ord, Show, Generic, Enum, Real, Fractional, Num, Serialize)

instance ToJSON a => ToJSON (Ratio a) where
  toJSON (Ratio x) = toJSON (R.numerator x, R.denominator x)

instance (FromJSON a, Integral a) => FromJSON (Ratio a) where
  parseJSON json = do
    (n,d) <- parseJSON json
    pure (Ratio (n R.% d))

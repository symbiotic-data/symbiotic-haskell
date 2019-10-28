{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int32.BE where

import Data.Int (Int32)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt32be)
import Data.Serialize.Put (putInt32be)
import Test.QuickCheck (Arbitrary)

newtype Int32BE = Int32BE {getInt32BE :: Int32}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Int32BE where
  get = Int32BE <$> getInt32be
  put (Int32BE x) = putInt32be x

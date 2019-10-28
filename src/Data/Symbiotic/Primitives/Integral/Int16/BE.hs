{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int16.BE where

import Data.Int (Int16)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt16be)
import Data.Serialize.Put (putInt16be)
import Test.QuickCheck (Arbitrary)

newtype Int16BE = Int16BE {getInt16BE :: Int16}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Int16BE where
  get = Int16BE <$> getInt16be
  put (Int16BE x) = putInt16be x

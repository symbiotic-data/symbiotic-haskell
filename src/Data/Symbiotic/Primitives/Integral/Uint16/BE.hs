{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint16.BE where

import Data.Word (Word16)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord16be)
import Data.Serialize.Put (putWord16be)
import Test.QuickCheck (Arbitrary)

newtype Uint16BE = Uint16BE {getUint16BE :: Word16}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Uint16BE where
  get = Uint16BE <$> getWord16be
  put (Uint16BE x) = putWord16be x

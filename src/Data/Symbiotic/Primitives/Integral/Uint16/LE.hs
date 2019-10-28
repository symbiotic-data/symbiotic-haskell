{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint16.LE where

import Data.Word (Word16)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord16le)
import Data.Serialize.Put (putWord16le)
import Test.QuickCheck (Arbitrary)

newtype Uint16LE = Uint16LE {getUint16LE :: Word16}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Uint16LE where
  get = Uint16LE <$> getWord16le
  put (Uint16LE x) = putWord16le x

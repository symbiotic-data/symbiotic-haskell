{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint32.LE where

import Data.Word (Word32)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord32le)
import Data.Serialize.Put (putWord32le)
import Test.QuickCheck (Arbitrary)

newtype Uint32LE = Uint32LE {getUint32LE :: Word32}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Uint32LE where
  get = Uint32LE <$> getWord32le
  put (Uint32LE x) = putWord32le x

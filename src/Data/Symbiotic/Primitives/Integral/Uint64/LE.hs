{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint64.LE where

import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord64le)
import Data.Serialize.Put (putWord64le)

newtype Uint64LE = Uint64LE {getUint64LE :: Word64}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Uint64LE where
  get = Uint64LE <$> getWord64le
  put (Uint64LE x) = putWord64le x

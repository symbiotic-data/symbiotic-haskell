{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint32.BE where

import Data.Word (Word32)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord32be)
import Data.Serialize.Put (putWord32be)

newtype Uint32BE = Uint32BE {getUint32BE :: Word32}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Uint32BE where
  get = Uint32BE <$> getWord32be
  put (Uint32BE x) = putWord32be x

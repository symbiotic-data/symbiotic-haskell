{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Uint64.BE where

import Data.Word (Word64)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord64be)
import Data.Serialize.Put (putWord64be)
import Test.QuickCheck (Arbitrary)

newtype Uint64BE = Uint64BE {getUint64BE :: Word64}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON, Arbitrary)

instance Serialize Uint64BE where
  get = Uint64BE <$> getWord64be
  put (Uint64BE x) = putWord64be x

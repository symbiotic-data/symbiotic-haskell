{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int64.LE where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt64le)
import Data.Serialize.Put (putInt64le)

newtype Int64LE = Int64LE {getInt64LE :: Int64}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Int64LE where
  get = Int64LE <$> getInt64le
  put (Int64LE x) = putInt64le x

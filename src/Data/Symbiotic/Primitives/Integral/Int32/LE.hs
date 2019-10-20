{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int32.LE where

import Data.Int (Int32)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt32le)
import Data.Serialize.Put (putInt32le)

newtype Int32LE = Int32LE {getInt32LE :: Int32}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Int32LE where
  get = Int32LE <$> getInt32le
  put (Int32LE x) = putInt32le x

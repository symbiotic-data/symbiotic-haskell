{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int16.LE where

import Data.Int (Int16)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt16le)
import Data.Serialize.Put (putInt16le)

newtype Int16LE = Int16LE {getInt16LE :: Int16}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Int16LE where
  get = Int16LE <$> getInt16le
  put (Int16LE x) = putInt16le x

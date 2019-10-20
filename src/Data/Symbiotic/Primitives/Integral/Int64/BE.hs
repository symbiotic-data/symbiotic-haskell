{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Int64.BE where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getInt64be)
import Data.Serialize.Put (putInt64be)

newtype Int64BE = Int64BE {getInt64BE :: Int64}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Int64BE where
  get = Int64BE <$> getInt64be
  put (Int64BE x) = putInt64be x

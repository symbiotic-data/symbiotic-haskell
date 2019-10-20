{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Boolean where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord8)
import Data.Serialize.Put (putWord8)

newtype Boolean = Boolean {getBoolean :: Bool}
  deriving (Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Boolean where
  get = do
    n <- getWord8
    case n of
      0 -> pure (Boolean False)
      1 -> pure (Boolean True)
      _ -> fail "Boolean"
  put (Boolean b) = putWord8 (if b then 1 else 0)

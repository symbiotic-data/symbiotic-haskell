{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  #-}

module Data.Symbiotic.Primitives.Unit where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (getWord8)
import Data.Serialize.Put (putWord8)

newtype Unit = Unit {getUnit :: ()}
  deriving (Generic, Eq, Show, Read, Ord)

instance ToJSON Unit where
  toJSON _ = String ""

instance FromJSON Unit where
  parseJSON json = case json of
    String s
      | s == "" -> pure (Unit ())
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "Unit" json

instance Serialize Unit where
  get = do
    i <- getWord8
    case i of
      0 -> pure (Unit ())
      _ -> fail "Unit"
  put _ = putWord8 0

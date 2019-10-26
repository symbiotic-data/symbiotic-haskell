{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String8 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (Vector8, makeVector8)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize)


newtype String8 = String8 {getString8 :: Vector8 [] Char}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Serialize)

instance IsString String8 where
  fromString s = case makeVector8 s of
    Nothing -> error "Vector8 can't be made from String"
    Just v -> String8 v

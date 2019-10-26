{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String64 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (Vector64, makeVector64)

import GHC.Generics (Generic)
import Data.String (IsString (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize)


newtype String64 = String64 {getString64 :: Vector64 [] Char}
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid, ToJSON, FromJSON, Serialize)

instance IsString String64 where
  fromString s = case makeVector64 s of
    Nothing -> error "Vector64 can't be made from String"
    Just v -> String64 v

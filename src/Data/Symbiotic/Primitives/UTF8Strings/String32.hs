{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String32 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (Vector32, makeVector32)

import GHC.Generics (Generic)
import Data.String (IsString (..))


newtype String32 = String32 (Vector32 [] Char)
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

instance IsString String32 where
  fromString s = case makeVector32 s of
    Nothing -> error "Vector32 can't be made from String"
    Just v -> String32 v

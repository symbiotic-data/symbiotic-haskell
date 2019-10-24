{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.UTF8Strings.String16 where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (Vector16, makeVector16)

import GHC.Generics (Generic)
import Data.String (IsString (..))


newtype String16 = String16 (Vector16 [] Char)
  deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

instance IsString String16 where
  fromString s = case makeVector16 s of
    Nothing -> error "Vector16 can't be made from String"
    Just v -> String16 v

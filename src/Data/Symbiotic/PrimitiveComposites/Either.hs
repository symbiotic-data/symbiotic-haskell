{-# LANGUAGE
    NoImplicitPrelude
  , DeriveGeneric
  , DeriveTraversable
  , OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.PrimitiveComposites.Either where

import Prelude hiding (Either)
import qualified Prelude as Prelude
import Data.Traversable (Traversable)
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=), Value (Object), object)
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)


newtype Either a b = Either (Prelude.Either a b)
  deriving (Generic, Show, Eq, Ord, Semigroup, Functor, Applicative, Monad, Foldable, Traversable, Serialize, Arbitrary)

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
  toJSON (Either e) = case e of
    Left x -> object ["l" .= x]
    Right y -> object ["r" .= y]

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
  parseJSON json = case json of
    Object o ->
      (Either . Left <$> o .: "l")
      <|> (Either . Right <$> o .: "r")
    _ -> typeMismatch "Either" json

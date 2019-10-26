{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Casual.URILike.EmailAddress where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (Vector16, getVector16, makeVector16)
import Data.Symbiotic.Primitives.UTF8Strings.String16 (String16 (..))

import qualified Text.EmailAddress as EA
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import GHC.Generics (Generic)


newtype EmailAddress = EmailAddress {getEmailAddress :: EA.EmailAddress}
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

instance Serialize EmailAddress where
  put (EmailAddress e) = case makeVector16 (T.unpack (EA.toText e)) of
    Nothing -> error "Vector16 can't be made from EmailAddress string"
    Just x -> put (String16 x)
  get = do
    s <- getVector16 . getString16 <$> get
    case EA.validateFromString s of
      Left e -> fail $ "EmailAddress: " ++ e
      Right e -> pure (EmailAddress e)

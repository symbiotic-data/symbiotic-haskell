{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Casual.URILike.EmailAddress where

import Data.Symbiotic.Primitives.UTF8Strings.String16 (getString16, makeString16)

import qualified Text.EmailAddress as EA
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype EmailAddress = EmailAddress {getEmailAddress :: EA.EmailAddress}
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

instance Serialize EmailAddress where
  put (EmailAddress e) = case makeString16 (EA.toText e) of
    Nothing -> error "Vector16 can't be made from EmailAddress string"
    Just x -> put x
  get = do
    s <- getString16 <$> get
    case EA.validateFromText s of
      Left e -> fail $ "EmailAddress: " ++ e
      Right e -> pure (EmailAddress e)
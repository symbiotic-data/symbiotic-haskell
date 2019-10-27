{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.URILike.URI where

import Data.Symbiotic.Primitives.UTF8Strings.String32 (getString32, makeString32)

import qualified Network.URI as URI
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype URI = URI {getURI :: URI.URI}
  deriving (Generic, Show, Eq, Ord)

instance ToJSON URI where
  toJSON (URI u) = String $ T.pack $ URI.uriToString id u ""

instance FromJSON URI where
  parseJSON json = case json of
    String s -> case URI.parseURI (T.unpack s) of
      Nothing -> fail'
      Just u -> pure (URI u)
    _ -> fail'
    where
      fail' = typeMismatch "URI" json

instance Serialize URI where
  put (URI u) = case makeString32 (T.pack (URI.uriToString id u "")) of
    Nothing -> error "Vector32 can't be made from URI string"
    Just x -> put x
  get = do
    s <- getString32 <$> get
    case URI.parseURI (T.unpack s) of
      Nothing -> fail "URI"
      Just u -> pure (URI u)

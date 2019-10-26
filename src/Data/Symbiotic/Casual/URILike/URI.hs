{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.URILike.URI where

import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (Vector32, getVector32, makeVector32)
import Data.Symbiotic.Primitives.UTF8Strings.String32 (String32 (..))

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
  put (URI u) = case makeVector32 (URI.uriToString id u "") of
    Nothing -> error "Vector32 can't be made from URI string"
    Just x -> put (String32 x)
  get = do
    s <- getVector32 . getString32 <$> get
    case URI.parseURI s of
      Nothing -> fail "URI"
      Just u -> pure (URI u)

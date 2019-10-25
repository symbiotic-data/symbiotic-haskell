{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap64 where

import Data.Symbiotic.Primitives.UTF8Strings.String64 (String64 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (getVector64, makeVector64)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, Value (Object), Result (..))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype StringMap64 a = StringMap64 {getStringMap64 :: Map String64 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StringMap64 a) where
  toJSON (StringMap64 xs) = Object $
    HM.fromList $
      map (\(String64 k,v) -> (T.pack (getVector64 k), toJSON v)) $
        Map.toList xs

instance FromJSON a => FromJSON (StringMap64 a) where
  parseJSON json = case json of
    Object o -> do
      let xs = HM.toList o
          go (k,v) = do
            v' <- parseJSON v
            case makeVector64 (T.unpack k) of
              Nothing -> fail'
              Just k' -> pure (String64 k',v')
      xs' <- traverse go xs
      pure (StringMap64 (Map.fromList xs'))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap64" json

instance Serialize a => Serialize (StringMap64 a) where
  put (StringMap64 x) = case makeVector64 (Map.toList x) of
    Nothing -> error "Serialize - can't make Vector64 out of StringMap64"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap64 (Map.fromList (getVector64 x)))

{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap32 where

import Data.Symbiotic.Primitives.UTF8Strings.String32 (String32 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (getVector32, makeVector32)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, Value (Object), Result (..))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype StringMap32 a = StringMap32 {getStringMap32 :: Map String32 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StringMap32 a) where
  toJSON (StringMap32 xs) = Object $
    HM.fromList $
      map (\(String32 k,v) -> (T.pack (getVector32 k), toJSON v)) $
        Map.toList xs

instance FromJSON a => FromJSON (StringMap32 a) where
  parseJSON json = case json of
    Object o -> do
      let xs = HM.toList o
          go (k,v) = do
            v' <- parseJSON v
            case makeVector32 (T.unpack k) of
              Nothing -> fail'
              Just k' -> pure (String32 k',v')
      xs' <- traverse go xs
      pure (StringMap32 (Map.fromList xs'))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap32" json

instance Serialize a => Serialize (StringMap32 a) where
  put (StringMap32 x) = case makeVector32 (Map.toList x) of
    Nothing -> error "Serialize - can't make Vector32 out of StringMap32"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap32 (Map.fromList (getVector32 x)))

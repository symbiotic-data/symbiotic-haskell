{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap8 where

import Data.Symbiotic.Primitives.UTF8Strings.String8 (String8 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (getVector8, makeVector8)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, Value (Object), Result (..))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype StringMap8 a = StringMap8 {getStringMap8 :: Map String8 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StringMap8 a) where
  toJSON (StringMap8 xs) = Object $
    HM.fromList $
      map (\(String8 k,v) -> (T.pack (getVector8 k), toJSON v)) $
        Map.toList xs

instance FromJSON a => FromJSON (StringMap8 a) where
  parseJSON json = case json of
    Object o -> do
      let xs = HM.toList o
          go (k,v) = do
            v' <- parseJSON v
            case makeVector8 (T.unpack k) of
              Nothing -> fail'
              Just k' -> pure (String8 k',v')
      xs' <- traverse go xs
      pure (StringMap8 (Map.fromList xs'))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap8" json

instance Serialize a => Serialize (StringMap8 a) where
  put (StringMap8 x) = case makeVector8 (Map.toList x) of
    Nothing -> error "Serialize - can't make Vector8 out of StringMap8"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap8 (Map.fromList (getVector8 x)))

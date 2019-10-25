{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , DeriveTraversable
  #-}

module Data.Symbiotic.SophisticatedComposites.Mappings.StringMap16 where

import Data.Symbiotic.Primitives.UTF8Strings.String16 (String16 (..))
import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (getVector16, makeVector16)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import Data.Aeson (ToJSON (..), FromJSON (..), fromJSON, Value (Object), Result (..))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype StringMap16 a = StringMap16 {getStringMap16 :: Map String16 a}
  deriving (Generic, Eq, Ord, Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (StringMap16 a) where
  toJSON (StringMap16 xs) = Object $
    HM.fromList $
      map (\(String16 k,v) -> (T.pack (getVector16 k), toJSON v)) $
        Map.toList xs

instance FromJSON a => FromJSON (StringMap16 a) where
  parseJSON json = case json of
    Object o -> do
      let xs = HM.toList o
          go (k,v) = do
            v' <- parseJSON v
            case makeVector16 (T.unpack k) of
              Nothing -> fail'
              Just k' -> pure (String16 k',v')
      xs' <- traverse go xs
      pure (StringMap16 (Map.fromList xs'))
    _ -> fail'
    where
      fail' = typeMismatch "StringMap16" json

instance Serialize a => Serialize (StringMap16 a) where
  put (StringMap16 x) = case makeVector16 (Map.toList x) of
    Nothing -> error "Serialize - can't make Vector16 out of StringMap16"
    Just y -> put y
  get = do
    x <- get
    pure (StringMap16 (Map.fromList (getVector16 x)))

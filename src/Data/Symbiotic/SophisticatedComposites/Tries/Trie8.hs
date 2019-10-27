{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.SophisticatedComposites.Tries.Trie8 where

import Data.Symbiotic.SophisticatedComposites.Mappings.Map8 (Map8 (..))

import qualified Data.Trie.Map as MT
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson.Types (typeMismatch, Parser, Value)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)


newtype Trie8 k a = Trie8 {getTrie8 :: MT.MapTrie k a}
  deriving (Generic, Show, Eq, Ord, Serialize)

instance (ToJSON k, ToJSON a) => ToJSON (Trie8 k a) where
  toJSON (Trie8 xs) = trie xs
    where
      trie (MT.MapTrie ys) = step ys
      step (MT.MapStep ys) = toJSON (Map.toList (children <$> ys))
      children (MT.MapChildren my ys) = toJSON [toJSON <$> my, trie <$> ys]

instance (FromJSON k, FromJSON a, Ord k) => FromJSON (Trie8 k a) where
  parseJSON json = Trie8 <$> trie json
    where
      trie j = MT.MapTrie <$> step j
      step j = do
        xs <- parseJSON j
        let xs' = Map.fromList xs
        xs'' <- traverse children xs'
        pure (MT.MapStep xs'')
      children j = do
        xs <- parseJSON j :: Parser (V.Vector (Maybe Value))
        if length xs /= 2
          then fail'
          else do
            my <- sequenceA (parseJSON <$> (xs V.! 0))
            ys <- sequenceA (trie <$> (xs V.! 1))
            pure (MT.MapChildren my ys)

      fail' = typeMismatch "Trie8" json

-- instance (Serialize k, Serialize a) => Serialize (Trie8 k a) where
--   put (Trie8 xs) =

instance (Serialize a, Serialize (c p a)) => Serialize (MT.MapChildren c p a) where
  put (MT.MapChildren my ys) = do
    put my
    put ys
  get = do
    my <- get
    ys <- get
    pure (MT.MapChildren my ys)

instance (Serialize a, Serialize p, Ord p, Serialize (c p a)) => Serialize (MT.MapStep c p a) where
  put (MT.MapStep xs) = put (Map8 xs)
  get = do
    Map8 xs <- get
    pure (MT.MapStep xs)

instance (Serialize a, Serialize k, Ord k) => Serialize (MT.MapTrie k a) where
  put (MT.MapTrie xs) = put xs
  get = MT.MapTrie <$> get

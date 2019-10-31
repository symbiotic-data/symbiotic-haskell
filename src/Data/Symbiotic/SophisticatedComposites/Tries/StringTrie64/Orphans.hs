{-# LANGUAGE
    FlexibleInstances
  #-}

module Data.Symbiotic.SophisticatedComposites.Tries.StringTrie64.Orphans where

import Data.Symbiotic.Primitives.UTF8Strings.String64 (String64)
import Data.Symbiotic.SophisticatedComposites.Mappings.StringMap64 (StringMap64 (..))
import qualified Data.Trie.HashMap as HT
import Data.Serialize (Serialize (..))

instance (Serialize a, Serialize (c p a)) => Serialize (HT.HashMapChildren c p a) where
  put (HT.HashMapChildren my ys) = do
    put my
    put ys
  get = do
    my <- get
    ys <- get
    pure (HT.HashMapChildren my ys)

instance (Serialize a, Serialize (c String64 a)) => Serialize (HT.HashMapStep c String64 a) where
  put (HT.HashMapStep xs) = put (StringMap64 xs)
  get = do
    StringMap64 xs <- get
    pure (HT.HashMapStep xs)

instance (Serialize a) => Serialize (HT.HashMapTrie String64 a) where
  put (HT.HashMapTrie xs) = put xs
  get = HT.HashMapTrie <$> get

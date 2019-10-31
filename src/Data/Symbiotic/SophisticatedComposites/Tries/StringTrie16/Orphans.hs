{-# LANGUAGE
    FlexibleInstances
  #-}

module Data.Symbiotic.SophisticatedComposites.Tries.StringTrie16.Orphans where

import Data.Symbiotic.Primitives.UTF8Strings.String16 (String16)
import Data.Symbiotic.SophisticatedComposites.Mappings.StringMap16 (StringMap16 (..))
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

instance (Serialize a, Serialize (c String16 a)) => Serialize (HT.HashMapStep c String16 a) where
  put (HT.HashMapStep xs) = put (StringMap16 xs)
  get = do
    StringMap16 xs <- get
    pure (HT.HashMapStep xs)

instance (Serialize a) => Serialize (HT.HashMapTrie String16 a) where
  put (HT.HashMapTrie xs) = put xs
  get = HT.HashMapTrie <$> get

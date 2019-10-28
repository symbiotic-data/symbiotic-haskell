module Data.Symbiotic.SophisticatedComposites.Tries.Trie64.Orphans where

import Data.Symbiotic.SophisticatedComposites.Mappings.Map64 (Map64 (..))
import qualified Data.Trie.Map as MT
import Data.Serialize (Serialize (..))

instance (Serialize a, Serialize (c p a)) => Serialize (MT.MapChildren c p a) where
  put (MT.MapChildren my ys) = do
    put my
    put ys
  get = do
    my <- get
    ys <- get
    pure (MT.MapChildren my ys)

instance (Serialize a, Serialize p, Ord p, Serialize (c p a)) => Serialize (MT.MapStep c p a) where
  put (MT.MapStep xs) = put (Map64 xs)
  get = do
    Map64 xs <- get
    pure (MT.MapStep xs)

instance (Serialize a, Serialize k, Ord k) => Serialize (MT.MapTrie k a) where
  put (MT.MapTrie xs) = put xs
  get = MT.MapTrie <$> get

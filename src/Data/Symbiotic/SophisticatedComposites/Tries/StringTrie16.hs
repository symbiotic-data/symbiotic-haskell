{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.SophisticatedComposites.Tries.StringTrie16 where

import Data.Symbiotic.SophisticatedComposites.Tries.StringTrie16.Orphans ()
import Data.Symbiotic.Primitives.UTF8Strings.String16 (String16)

import qualified Data.Trie.HashMap as HT
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object))
import Data.Aeson.Types (typeMismatch, Parser)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (atMost', arbitraryMaybe)
import Unsafe.Coerce (unsafeCoerce)


newtype StringTrie16 a = StringTrie16 {getStringTrie16 :: HT.HashMapTrie String16 a}
  deriving (Generic, Show, Eq, Serialize)

instance (Arbitrary a) => Arbitrary (StringTrie16 a) where
  arbitrary = StringTrie16 <$> trie
    where
      trie = HT.HashMapTrie <$> step
      step =
        let elems = (,) <$> arbitrary <*> children
        in  HT.HashMapStep . HM.fromList <$> atMost' ((2 :: Int) ^ (8 :: Int)) elems
      children = HT.HashMapChildren <$> arbitrary <*> arbitraryMaybe trie

instance (ToJSON a) => ToJSON (StringTrie16 a) where
  toJSON (StringTrie16 xs) = trie xs
    where
      trie (HT.HashMapTrie ys) = step ys
      step (HT.HashMapStep ys) = Object (unsafeCoerce (children <$> ys))
      children (HT.HashMapChildren my ys) = toJSON [toJSON <$> my, trie <$> ys]

instance (FromJSON a) => FromJSON (StringTrie16 a) where
  parseJSON json = StringTrie16 <$> trie json
    where
      trie j = HT.HashMapTrie <$> step j
      step j = case j of
        Object xs' -> do
          xs'' <- traverse children xs'
          pure (HT.HashMapStep (unsafeCoerce xs''))
        _ -> fail'
      children j = do
        xs <- parseJSON j :: Parser (V.Vector (Maybe Value))
        if length xs /= 2
          then fail'
          else do
            my <- sequenceA (parseJSON <$> (xs V.! 0))
            ys <- sequenceA (trie <$> (xs V.! 1))
            pure (HT.HashMapChildren my ys)

      fail' = typeMismatch "HashTrie16" json

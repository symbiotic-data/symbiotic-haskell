module Test.QuickCheck.Arbitrary.Limited where

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen, resize, listOf, choose, elements)
import Data.Bits (Bits)
import Data.Symbiotic.Primitives.Integral.Integer.Utils (roll)
import Control.Monad (replicateM)


atMost :: Arbitrary a => Int -> Gen [a]
atMost n = atMost' n arbitrary

atMost' :: Int -> Gen a -> Gen [a]
atMost' n x = do -- resize n (listOf x)
  l <- choose (0,n-1)
  replicateM l x

arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe x = do
  m <- arbitrary
  if m then pure Nothing else Just <$> x

maxInteger :: (Integral a, Bits a) => Int -> Gen a
maxInteger n = do
  l <- choose (0, n)
  xs <- replicateM l arbitrary
  shouldNegate <- do
    w <- arbitrary
    pure $ if w then negate else id
  pure (shouldNegate (roll xs))

maxNatural :: (Integral a, Bits a) => Int -> Gen a
maxNatural n = do
  l <- choose (0, n)
  xs <- replicateM l arbitrary
  pure (roll xs)

asciiAtMost :: Int -> Gen String
asciiAtMost n = do
  l <- choose (0, n)
  replicateM l $ elements $ ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

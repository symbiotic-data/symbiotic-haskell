module Test.QuickCheck.Arbitrary.Limited where

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen, resize, listOf, choose, elements)
import Data.Bits (Bits)
import Data.Symbiotic.Primitives.Integral.Integer.Utils (roll)
import Control.Monad (replicateM)


atMost :: Arbitrary a => Int -> Gen [a]
atMost n = resize n (listOf arbitrary)

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

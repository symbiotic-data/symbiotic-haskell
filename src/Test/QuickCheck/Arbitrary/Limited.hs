module Test.QuickCheck.Arbitrary.Limited where

import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen, resize, listOf)


atMost :: Arbitrary a => Int -> Gen [a]
atMost n = resize n (listOf arbitrary)

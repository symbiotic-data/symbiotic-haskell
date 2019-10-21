module Data.Symbiotic.Primitives.Integral.Integer.Utils where

import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.|.))
import Data.List (unfoldr)


-- | From http://hackage.haskell.org/package/cereal-0.5.8.1/docs/src/Data.Serialize.html#line-246
unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

-- | From http://hackage.haskell.org/package/cereal-0.5.8.1/docs/src/Data.Serialize.html#line-246
roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

-- | Disregards sign byte
nrBytes :: Integer -> Int
nrBytes n = (nrBits (abs n) + 7) `div` 8

-- | Returns the number of bits in the tail of the Integer, from http://hackage.haskell.org/package/cereal-0.5.8.1/docs/src/Data.Serialize.html#line-246
nrBits :: Integer -> Int
nrBits k =
  let expMax = until (\e -> 2 ^ e > k) (* 2) 1
      findNr :: Int -> Int -> Int
      findNr lo hi
        | mid == lo = hi
        | 2 ^ mid <= k = findNr mid hi
        | 2 ^ mid > k  = findNr lo mid
        | otherwise = error "impossible case"
        where
          mid = (lo + hi) `div` 2
  in findNr (expMax `div` 2) expMax

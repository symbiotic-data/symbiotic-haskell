{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Integer8
  ( Integer8, getInteger8, makeInteger8
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (Vector8 (getVector8))

import GHC.Generics (Generic)
import Data.Bits (Bits)
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8)
import qualified Data.Vector as V
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxInteger)

newtype Integer8 = Integer8 {getInteger8 :: Integer}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Arbitrary Integer8 where
  arbitrary = maxInteger 8

instance Serialize Integer8 where
  put n | n >= lo && n <= hi = do
    putWord8 0
    put (fromIntegral n :: Int32)  -- fast path
    where
      lo :: Integer8
      lo = fromIntegral (minBound :: Int32)
      hi :: Integer8
      hi = fromIntegral (maxBound :: Int32)

  put (Integer8 n) = do
    putWord8 1
    put sign
    let len :: Word8
        len = fromIntegral (nrBytes n)
    putWord8 len                         -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes
    where
      sign :: Word8
      sign = fromIntegral (signum n)

  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> fromIntegral <$> (get :: Get Int32)
      _ -> do
        sign  <- get
        bytes <- getVector8 <$> get      -- NOTE point of interest
        let v :: Integer8
            v = Integer8 (roll (V.toList bytes))
        return $! if sign == (1 :: Word8) then v else - v

makeInteger8 :: Integer -> Maybe Integer8
makeInteger8 x
  | nrBytes x <= 8 = Just (Integer8 x)
  | otherwise = Nothing

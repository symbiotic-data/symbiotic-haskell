{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Integer64
  ( Integer64, getInteger64, makeInteger64
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (Vector64 (getVector64))

import GHC.Generics (Generic)
import Data.Int (Int32)
import Data.Word (Word64, Word8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord64be, getWord8)
import Data.Serialize.Put (putWord64be, putWord8)
import Control.Monad (void)

newtype Integer64 = Integer64 {getInteger64 :: Integer}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Integer64 where
  put n | n >= lo && n <= hi = do
    putWord8 0
    put (fromIntegral n :: Int32)  -- fast path
    where
      lo :: Integer64
      lo = fromIntegral (minBound :: Int32)
      hi :: Integer64
      hi = fromIntegral (maxBound :: Int32)

  put (Integer64 n) = do
    putWord8 1
    putWord8 sign
    let len :: Word64
        len = fromIntegral (nrBytes n)
    putWord64be len                      -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes
    where
      sign :: Word8
      sign = fromIntegral (signum n)

  get = do
    tag <- getWord8
    case tag of
      0 -> fromIntegral <$> (get :: Get Int32)
      _ -> do
        sign  <- getWord8
        bytes <- getVector64 <$> get     -- NOTE point of interest
        let v :: Integer64
            v = Integer64 (roll bytes)
        return $! if sign == (1 :: Word8) then v else - v

makeInteger64 :: Integer -> Maybe Integer64
makeInteger64 x
  | nrBytes x <= 64 = Just (Integer64 x)
  | otherwise = Nothing

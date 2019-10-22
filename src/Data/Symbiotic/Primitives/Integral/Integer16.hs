{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Integer16
  ( Integer16, getInteger16, makeInteger16
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (Vector16 (getVector16))

import GHC.Generics (Generic)
import Data.Int (Int32)
import Data.Word (Word16, Word8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord16be, getWord8)
import Data.Serialize.Put (putWord16be, putWord8)
import Control.Monad (void)

newtype Integer16 = Integer16 {getInteger16 :: Integer}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Integer16 where
  put n | n >= lo && n <= hi = do
    putWord8 0
    put (fromIntegral n :: Int32)  -- fast path
    where
      lo :: Integer16
      lo = fromIntegral (minBound :: Int32)
      hi :: Integer16
      hi = fromIntegral (maxBound :: Int32)

  put (Integer16 n) = do
    putWord8 1
    putWord8 sign
    let len :: Word16
        len = fromIntegral (nrBytes n)
    putWord16be len                      -- NOTE point of interest
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
        bytes <- getVector16 <$> get     -- NOTE point of interest
        let v :: Integer16
            v = Integer16 (roll bytes)
        return $! if sign == (1 :: Word8) then v else - v

makeInteger16 :: Integer -> Maybe Integer16
makeInteger16 x
  | nrBytes x <= 16 = Just (Integer16 x)
  | otherwise = Nothing

{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Integer32
  ( Integer32, getInteger32, makeInteger32
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (Vector32 (getVector32))

import GHC.Generics (Generic)
import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord32be, getWord8)
import Data.Serialize.Put (putWord32be, putWord8)
import Control.Monad (void)

newtype Integer32 = Integer32 {getInteger32 :: Integer}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Integer32 where
  put n | n >= lo && n <= hi = do
    putWord8 0
    put (fromIntegral n :: Int32)  -- fast path
    where
      lo :: Integer32
      lo = fromIntegral (minBound :: Int32)
      hi :: Integer32
      hi = fromIntegral (maxBound :: Int32)

  put (Integer32 n) = do
    putWord8 1
    putWord8 sign
    let len :: Word32
        len = fromIntegral (nrBytes n)
    putWord32be len                      -- NOTE point of interest
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
        bytes <- getVector32 <$> get     -- NOTE point of interest
        let v :: Integer32
            v = Integer32 (roll bytes)
        return $! if sign == (1 :: Word8) then v else - v

makeInteger32 :: Integer -> Maybe Integer32
makeInteger32 x
  | nrBytes x <= 32 = Just (Integer32 x)
  | otherwise = Nothing
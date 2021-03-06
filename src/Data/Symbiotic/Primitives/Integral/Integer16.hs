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
import Data.Bits (Bits)
import Data.Int (Int32)
import Data.Word (Word16, Word8)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord16be, putWord8)
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxInteger)

newtype Integer16 = Integer16 {getInteger16 :: Integer}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord)

instance ToJSON Integer16 where
  toJSON x = String (T.pack (show x))

instance FromJSON Integer16 where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "Integer16" json

instance Arbitrary Integer16 where
  arbitrary = maxInteger 16

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
            v = Integer16 (roll (V.toList bytes))
        return $! if sign == (1 :: Word8) then v else - v

makeInteger16 :: Integer -> Maybe Integer16
makeInteger16 x
  | nrBytes x <= 16 = Just (Integer16 x)
  | otherwise = Nothing

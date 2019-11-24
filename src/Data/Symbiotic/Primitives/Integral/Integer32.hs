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
import Data.Bits (Bits)
import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord32be, putWord8)
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxInteger)

newtype Integer32 = Integer32 {getInteger32 :: Integer}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord)

instance ToJSON Integer32 where
  toJSON x = String (T.pack (show x))

instance FromJSON Integer32 where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "Integer32" json

instance Arbitrary Integer32 where
  arbitrary = maxInteger 32

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
            v = Integer32 (roll (V.toList bytes))
        return $! if sign == (1 :: Word8) then v else - v

makeInteger32 :: Integer -> Maybe Integer32
makeInteger32 x
  | nrBytes x <= 32 = Just (Integer32 x)
  | otherwise = Nothing

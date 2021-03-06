{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Natural32
  ( Natural32, getNatural32, makeNatural32
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector32 (Vector32 (getVector32))

import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Data.Bits (Bits)
import Data.Word (Word32)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8, putWord32be)
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxNatural)

newtype Natural32 = Natural32 {getNatural32 :: Natural}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord)

instance ToJSON Natural32 where
  toJSON x = String (T.pack (show x))

instance FromJSON Natural32 where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "Natural32" json

instance Arbitrary Natural32 where
  arbitrary = maxNatural 32

instance Serialize Natural32 where
  put n | n <= hi = do
    putWord8 0
    put (fromIntegral n :: Word32)  -- fast path
    where
      hi :: Natural32
      hi = fromIntegral (maxBound :: Word32)

  put (Natural32 n) = do
    putWord8 1
    let len :: Word32
        len = fromIntegral (nrBytes n)
    putWord32be len                      -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes

  get = do
    tag <- getWord8
    case tag of
      0 -> fromIntegral <$> (get :: Get Word32)
      _ -> do
        bytes <- getVector32 <$> get      -- NOTE point of interest
        return $! Natural32 (roll (V.toList bytes))

makeNatural32 :: Natural -> Maybe Natural32
makeNatural32 x
  | nrBytes x <= 32 = Just (Natural32 x)
  | otherwise = Nothing

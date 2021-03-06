{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Natural8
  ( Natural8, getNatural8, makeNatural8
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector8 (Vector8 (getVector8))

import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Data.Bits (Bits)
import Data.Word (Word8, Word32)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8)
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxNatural)

newtype Natural8 = Natural8 {getNatural8 :: Natural}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord)

instance ToJSON Natural8 where
  toJSON x = String (T.pack (show x))

instance FromJSON Natural8 where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "Natural8" json

instance Arbitrary Natural8 where
  arbitrary = maxNatural 8

instance Serialize Natural8 where
  put n | n <= hi = do
    putWord8 0
    put (fromIntegral n :: Word32)  -- fast path
    where
      hi :: Natural8
      hi = fromIntegral (maxBound :: Word32)

  put (Natural8 n) = do
    putWord8 1
    let len :: Word8
        len = fromIntegral (nrBytes n)
    putWord8 len                         -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes

  get = do
    tag <- getWord8
    case tag of
      0 -> fromIntegral <$> (get :: Get Word32)
      _ -> do
        bytes <- getVector8 <$> get      -- NOTE point of interest
        return $! Natural8 (roll (V.toList bytes))

makeNatural8 :: Natural -> Maybe Natural8
makeNatural8 x
  | nrBytes x <= 8 = Just (Natural8 x)
  | otherwise = Nothing

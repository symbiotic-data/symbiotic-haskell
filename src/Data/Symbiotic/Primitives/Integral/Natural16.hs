{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Natural16
  ( Natural16, getNatural16, makeNatural16
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector16 (Vector16 (getVector16))

import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Data.Bits (Bits)
import Data.Word (Word16, Word32)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8, putWord16be)
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (void)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Limited (maxNatural)

newtype Natural16 = Natural16 {getNatural16 :: Natural}
  deriving (Bits, Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord)

instance ToJSON Natural16 where
  toJSON x = String (T.pack (show x))

instance FromJSON Natural16 where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Nothing -> fail'
      Just x -> pure x
    _ -> fail'
    where
      fail' = typeMismatch "Natural16" json

instance Arbitrary Natural16 where
  arbitrary = maxNatural 16

instance Serialize Natural16 where
  put n | n <= hi = do
    putWord8 0
    put (fromIntegral n :: Word32)  -- fast path
    where
      hi :: Natural16
      hi = fromIntegral (maxBound :: Word32)

  put (Natural16 n) = do
    putWord8 1
    let len :: Word16
        len = fromIntegral (nrBytes n)
    putWord16be len                      -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes

  get = do
    tag <- getWord8
    case tag of
      0 -> fromIntegral <$> (get :: Get Word32)
      _ -> do
        bytes <- getVector16 <$> get      -- NOTE point of interest
        return $! Natural16 (roll (V.toList bytes))

makeNatural16 :: Natural -> Maybe Natural16
makeNatural16 x
  | nrBytes x <= 16 = Just (Natural16 x)
  | otherwise = Nothing

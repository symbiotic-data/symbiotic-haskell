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
import Data.Word (Word8, Word32)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8)
import qualified Data.Vector as V
import Control.Monad (void)

newtype Natural8 = Natural8 {getNatural8 :: Natural}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

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

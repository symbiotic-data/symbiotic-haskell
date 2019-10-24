{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Integral.Natural64
  ( Natural64, getNatural64, makeNatural64
  ) where

import Data.Symbiotic.Primitives.Integral.Integer.Utils (nrBytes, unroll, roll)
import Data.Symbiotic.PrimitiveComposites.Collections.Vector64 (Vector64 (getVector64))

import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Data.Word (Word64, Word32)
import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize (..))
import Data.Serialize.Get (Get, getWord8)
import Data.Serialize.Put (putWord8, putWord64be)
import Control.Monad (void)

newtype Natural64 = Natural64 {getNatural64 :: Natural}
  deriving (Enum, Real, Integral, Num, Generic, Eq, Show, Read, Ord, ToJSON, FromJSON)

instance Serialize Natural64 where
  put n | n <= hi = do
    putWord8 0
    put (fromIntegral n :: Word32)  -- fast path
    where
      hi :: Natural64
      hi = fromIntegral (maxBound :: Word32)

  put (Natural64 n) = do
    putWord8 1
    let len :: Word64
        len = fromIntegral (nrBytes n)
    putWord64be len                      -- NOTE point of interest
    void (traverse put (unroll (abs n))) -- unroll the bytes

  get = do
    tag <- getWord8
    case tag of
      0 -> fromIntegral <$> (get :: Get Word32)
      _ -> do
        bytes <- getVector64 <$> get      -- NOTE point of interest
        return $! Natural64 (roll bytes)

makeNatural64 :: Natural -> Maybe Natural64
makeNatural64 x
  | nrBytes x <= 64 = Just (Natural64 x)
  | otherwise = Nothing

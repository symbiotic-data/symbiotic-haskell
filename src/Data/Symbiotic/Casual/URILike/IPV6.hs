{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.URILike.IPV6 where

import Net.IPv6 (IPv6, encode, decode, toWord16s, fromWord16s)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord16be)
import Data.Serialize.Get (getWord16be)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))


newtype IPV6 = IPV6 {getIPV6 :: IPv6}
  deriving (Generic, Eq, Ord, Show)

instance Arbitrary IPV6 where
  arbitrary = IPV6 <$>
    ( fromWord16s
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    )

instance ToJSON IPV6 where
  toJSON = String . encode . getIPV6

instance FromJSON IPV6 where
  parseJSON json = case json of
    String s -> case decode s of
      Nothing -> fail'
      Just x -> pure (IPV6 x)
    _ -> fail'
    where
      fail' = typeMismatch "IPV6" json

instance Serialize IPV6 where
  put (IPV6 ip) =
    let (a,b,c,d,e,f,g,h) = toWord16s ip
    in  putWord16be a
        *> putWord16be b
        *> putWord16be c
        *> putWord16be d
        *> putWord16be e
        *> putWord16be f
        *> putWord16be g
        *> putWord16be h
  get = IPV6 <$>
        ( fromWord16s
          <$> getWord16be
          <*> getWord16be
          <*> getWord16be
          <*> getWord16be
          <*> getWord16be
          <*> getWord16be
          <*> getWord16be
          <*> getWord16be
        )

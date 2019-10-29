{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.URILike.IPV4 where

import Net.IPv4 (IPv4, encode, decode, toOctets, fromOctets)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord8)
import Data.Serialize.Get (getWord8)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))


newtype IPV4 = IPV4 {getIPV4 :: IPv4}
  deriving (Generic, Eq, Ord, Show)

instance Arbitrary IPV4 where
  arbitrary = IPV4 <$> (fromOctets <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance ToJSON IPV4 where
  toJSON = String . encode . getIPV4

instance FromJSON IPV4 where
  parseJSON json = case json of
    String s -> case decode s of
      Nothing -> fail'
      Just x -> pure (IPV4 x)
    _ -> fail'
    where
      fail' = typeMismatch "IPV4" json

instance Serialize IPV4 where
  put (IPV4 ip) =
    let (a,b,c,d) = toOctets ip
    in  putWord8 a *> putWord8 b *> putWord8 c *> putWord8 d
  get = IPV4 <$> (fromOctets <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8)

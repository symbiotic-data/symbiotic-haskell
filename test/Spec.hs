import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary)
import Data.Proxy (Proxy (..))
import Data.Aeson (FromJSON, ToJSON (..), fromJSON)
import Data.Serialize (Serialize, encode, decode)
import Data.Typeable (Typeable, typeRep)

import qualified Data.Symbiotic.Primitives as Prim



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Local Isomorphisms"
    [ testGroup "Primitives"
      [ both (Proxy :: Proxy Prim.Unit)
      , both (Proxy :: Proxy Prim.String8)
      , both (Proxy :: Proxy Prim.String16)
      -- , both (Proxy :: Proxy Prim.String32)
      -- , both (Proxy :: Proxy Prim.String64)
      , both (Proxy :: Proxy Prim.Integer8)
      , both (Proxy :: Proxy Prim.Integer16)
      , both (Proxy :: Proxy Prim.Integer32)
      , both (Proxy :: Proxy Prim.Integer64)
      , both (Proxy :: Proxy Prim.Natural8)
      , both (Proxy :: Proxy Prim.Natural16)
      , both (Proxy :: Proxy Prim.Natural32)
      , both (Proxy :: Proxy Prim.Natural64)
      , both (Proxy :: Proxy Prim.Int16BE)
      , both (Proxy :: Proxy Prim.Int16LE)
      , both (Proxy :: Proxy Prim.Int32BE)
      , both (Proxy :: Proxy Prim.Int32LE)
      , both (Proxy :: Proxy Prim.Int64BE)
      , both (Proxy :: Proxy Prim.Int64LE)
      , both (Proxy :: Proxy Prim.Uint16BE)
      , both (Proxy :: Proxy Prim.Uint16LE)
      , both (Proxy :: Proxy Prim.Uint32BE)
      , both (Proxy :: Proxy Prim.Uint32LE)
      , both (Proxy :: Proxy Prim.Uint64BE)
      , both (Proxy :: Proxy Prim.Uint64LE)
      , both (Proxy :: Proxy Prim.Scientific)
      , both (Proxy :: Proxy Prim.Float32BE)
      , both (Proxy :: Proxy Prim.Float32LE)
      , both (Proxy :: Proxy Prim.Float64BE)
      , both (Proxy :: Proxy Prim.Float64LE)
      ]
    ]
  ]

both :: Arbitrary a
     => Typeable a
     => Eq a
     => Show a
     => FromJSON a
     => ToJSON a
     => Serialize a
     => Proxy a -> TestTree
both x = testGroup (show (typeRep x))
  [ testProperty "Json" (jsonIso x)
  , testProperty "Binary" (binaryIso x)
  ]

jsonIso :: Eq a => FromJSON a => ToJSON a => Proxy a -> a -> Bool
jsonIso Proxy x = fromJSON (toJSON x) == pure x

binaryIso :: Eq a => Serialize a => Proxy a -> a -> Bool
binaryIso Proxy x = decode (encode x) == pure x

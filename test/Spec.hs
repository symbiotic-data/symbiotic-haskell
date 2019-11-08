import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary)
import Data.Proxy (Proxy (..))
import Data.Aeson (FromJSON, ToJSON (..), fromJSON)
import Data.Serialize (Serialize, encode, decode)
import Data.Typeable (Typeable, typeRep)

import qualified Data.Symbiotic.Primitives as Prim
import qualified Data.Symbiotic.PrimitiveComposites as PrimCom
import qualified Data.Symbiotic.SophisticatedComposites as SophCom
import qualified Data.Symbiotic.Casual as Casual



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ testGroup "Local Isomorphisms"
    [ testGroup "Primitives"
      [ both (Proxy :: Proxy Prim.Unit)
      , both (Proxy :: Proxy Prim.String8)
      -- , both (Proxy :: Proxy Prim.String16)
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
    , testGroup "PrimitiveComposites"
      [ both (Proxy :: Proxy (PrimCom.Either Prim.Unit Prim.Unit))
      , both (Proxy :: Proxy (PrimCom.Vector8 Prim.Unit))
      , both (Proxy :: Proxy (PrimCom.Vector16 Prim.Unit))
      , both (Proxy :: Proxy (PrimCom.Vector32 Prim.Unit))
      , both (Proxy :: Proxy (PrimCom.Vector64 Prim.Unit))
      ]
    , testGroup "SophisticatedComposites"
      [ both (Proxy :: Proxy (SophCom.Map8 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Map16 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Map32 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Map64 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringMap8 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringMap16 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringMap32 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringMap64 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Trie8 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Trie16 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Trie32 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.Trie64 Prim.Uint16BE Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringTrie8 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringTrie16 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringTrie32 Prim.Unit))
      , both (Proxy :: Proxy (SophCom.StringTrie64 Prim.Unit))
      ]
    , testGroup "Casual"
      [ both (Proxy :: Proxy Casual.Date)
      , both (Proxy :: Proxy Casual.Time)
      , both (Proxy :: Proxy Casual.DateTime)
      , both (Proxy :: Proxy Casual.IPV4)
      , both (Proxy :: Proxy Casual.IPV6)
      , both (Proxy :: Proxy Casual.URI)
      , both (Proxy :: Proxy Casual.EmailAddress)
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

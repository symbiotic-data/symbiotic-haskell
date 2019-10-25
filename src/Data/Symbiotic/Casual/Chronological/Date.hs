{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Casual.Chronological.Date where

import Data.Time.Calendar (Day, toGregorian, fromGregorianValid)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord8, putInt16be)
import Data.Serialize.Get (getWord8, getInt16be)
import qualified Data.Text as T
import Data.Int (Int16)
import Data.Word (Word8)
import GHC.Generics (Generic)


newtype Date = Date {getDate :: Day}
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Date where
  toJSON (Date d) = String $ T.pack $ formatTime defaultTimeLocale "%0Y%m%d" d

instance FromJSON Date where
  parseJSON json = case json of
    String s -> Date <$> parseTimeM True defaultTimeLocale "%0Y%m%d" (T.unpack s)
    _ -> typeMismatch "Date" json

instance Serialize Date where
  put (Date x) =
    let (y,m,d) = toGregorian x
        y' :: Int16
        y' = fromIntegral y
        m' :: Word8
        m' = fromIntegral m
        d' :: Word8
        d' = fromIntegral d
    in  putInt16be y' *> putWord8 m' *> putWord8 d'
  get = do
    y' <- getInt16be
    m' <- getWord8
    d' <- getWord8
    case fromGregorianValid (fromIntegral y') (fromIntegral m') (fromIntegral d') of
      Nothing -> fail $ "Not a valid date: " ++ show y' ++ ", " ++ show m' ++ ", " ++ show d'
      Just x -> pure (Date x)

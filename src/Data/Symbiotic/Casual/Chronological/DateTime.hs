{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Data.Symbiotic.Casual.Chronological.DateTime where

import Data.Symbiotic.Casual.Chronological.Date (Date (..))
import Data.Symbiotic.Casual.Chronological.Time (makeTime, timeOfDay, timeZone)

import Data.Time
  ( UTCTime (..), timeToTimeOfDay, LocalTime (..), localTimeToUTC
  , picosecondsToDiffTime, diffTimeToPicoseconds, TimeZone (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()
import Text.Read (Read)


newtype DateTime = DateTime {getDateTime :: UTCTime}
  deriving (Generic, Eq, Ord, Show, Read)

instance Arbitrary DateTime where
  arbitrary = DateTime <$> (UTCTime <$> arbitrary <*> (fixTime <$> arbitrary))
    where
      fixTime = picosecondsToDiffTime . unMili . diffTimeToPicoseconds
        where
          unMili pico = (pico `div` 1000000000) * 1000000000

instance ToJSON DateTime where
  toJSON (DateTime (UTCTime d diff)) =
    let String dString = toJSON (Date d)
        String tString = toJSON
          (makeTime (timeToTimeOfDay diff) (TimeZone 0 False ""))
    in  String (dString <> "T" <> tString)

instance FromJSON DateTime where
  parseJSON json = case json of
    String s -> do
      let dString = T.take 8 s
          tString = T.drop 9 s
      Date day <- parseJSON (String dString)
      t <- parseJSON (String tString)
      pure (DateTime (localTimeToUTC (timeZone t) (LocalTime day (timeOfDay t))))
    _ -> typeMismatch "DateTime" json

instance Serialize DateTime where
  put (DateTime (UTCTime d diff)) =
    let tod = timeToTimeOfDay diff
        tz = TimeZone 0 False ""
    in  put (Date d) *> put (makeTime tod tz)
  get = do
    Date d <- get
    t <- get
    pure (DateTime (localTimeToUTC (timeZone t) (LocalTime d (timeOfDay t))))

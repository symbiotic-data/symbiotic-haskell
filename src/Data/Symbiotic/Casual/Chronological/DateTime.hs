{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Data.Symbiotic.Casual.Chronological.DateTime where

import Data.Symbiotic.Casual.Chronological.Date (Date (..))
import Data.Symbiotic.Casual.Chronological.Time (makeTime, timeOfDay, timeZone)

import Data.Time (UTCTime (..), timeToTimeOfDay, getTimeZone, LocalTime (..), localTimeToUTC)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()


newtype DateTime = DateTime {getDateTime :: UTCTime}
  deriving (Generic, Eq, Ord, Show, Arbitrary)

instance ToJSON DateTime where
  toJSON (DateTime dt@(UTCTime d diff)) =
    let String dString = toJSON (Date d)
        String tString = toJSON
          (makeTime (timeToTimeOfDay diff) (unsafePerformIO (getTimeZone dt)))
    in  String (dString <> "T" <> tString)

instance FromJSON DateTime where
  parseJSON json = case json of
    String s -> do
      let dString = T.take 10 s
          tString = T.drop 11 s
      Date day <- parseJSON (String dString)
      t <- parseJSON (String tString)
      pure (DateTime (localTimeToUTC (timeZone t) (LocalTime day (timeOfDay t))))
    _ -> typeMismatch "DateTime" json

instance Serialize DateTime where
  put (DateTime dt@(UTCTime d diff)) =
    let tod = timeToTimeOfDay diff
        tz = unsafePerformIO (getTimeZone dt)
    in  put (Date d) *> put (makeTime tod tz)
  get = do
    Date d <- get
    t <- get
    pure (DateTime (localTimeToUTC (timeZone t) (LocalTime d (timeOfDay t))))

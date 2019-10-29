{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.Chronological.Time
  ( Time, timeOfDay, timeZone, makeTime
  ) where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone (..), makeTimeOfDayValid)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import Data.Serialize.Put (putWord8, putInt8, putWord16be)
import Data.Serialize.Get (getWord8, getInt8, getWord16be)
import qualified Data.Text as T
import Data.Fixed (Fixed (..))
import Data.Int (Int8)
import Data.Word (Word8)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


data Time = Time
  { timeOfDay :: TimeOfDay
  , timeZone  :: TimeZone
  }
  deriving (Generic, Eq, Ord, Show)

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary <*> arbitrary

-- | Haskell makes use of picoseconds, which are of no use to us in this library
makeTime :: TimeOfDay -> TimeZone -> Time
makeTime (TimeOfDay hour minute (MkFixed pico)) =
  Time (TimeOfDay hour minute (MkFixed unMili))
  where
    unMili = (pico `div` 1000000000) * 1000000000

instance ToJSON Time where
  toJSON (Time timeOfDay'@(TimeOfDay _ _ (MkFixed pico)) timeZone') = String $
    let t = T.pack (formatTime defaultTimeLocale "%H%M" timeOfDay')
        tz = T.pack (formatTime defaultTimeLocale "%z" timeZone')
        mili :: Integer
        mili = pico `div` 1000000000
        mili' :: Float
        mili' = fromIntegral mili / 1000
        pre0Mili
          | mili < 10 = "0" ++ show mili'
          | otherwise = show mili'
        shownMili
          | miliLen == 2 = pre0Mili ++ ".000"
          | miliLen < 6 = pre0Mili ++ "." ++ replicate (5 - miliLen) '0'
          | otherwise = take 6 pre0Mili
          where
            miliLen = length pre0Mili
    in  t <> T.pack shownMili <> tz

instance FromJSON Time where
  parseJSON json = case json of
    String s -> do
      let tString = T.unpack (T.take 4 s)
      t <- parseTimeM True defaultTimeLocale "%H%M" tString
      let miliString = T.unpack (T.take 6 (T.drop 4 s))
      mili <- case readMaybe miliString of
        Nothing -> fail'
        Just m' -> pure (m' :: Float)
      let tzString = T.unpack (T.drop 10 s)
      tz <- parseTimeM True defaultTimeLocale "%z" tzString
      let mili' :: Integer
          mili' = floor (mili * 1000)
      let timeOfDay' = t {todSec = MkFixed (mili' * 1000000000)}
      pure (Time timeOfDay' tz)
    _ -> fail'
    where
      fail' = typeMismatch "Time" json

instance Serialize Time where
  put (Time (TimeOfDay hour minute (MkFixed pico)) (TimeZone mins _ _)) =
    let tzHour :: Int8
        tzHour = fromIntegral (mins `div` 60)
        tzMin :: Word8
        tzMin = fromIntegral (mins `mod` 60)
        mili :: Integer
        mili = pico `div` 1000000000
        sec :: Integer
        sec = mili `div` 1000
    in  putInt8 tzHour
        *> putWord8 tzMin
        *> putWord8 (fromIntegral hour)
        *> putWord8 (fromIntegral minute)
        *> putWord8 (fromIntegral sec)
        *> putWord16be (fromIntegral (mili `mod` 1000))
  get = do
    tzHour <- getInt8
    tzMin <- getWord8
    hour <- getWord8
    minute <- getWord8
    sec <- getWord8
    mili <- getWord16be
    let tz = TimeZone ((fromIntegral tzHour * 60) + fromIntegral tzMin) False ""
        mili' :: Integer
        mili' = (fromIntegral sec * 1000) + fromIntegral mili
    timeOfDay' <-
      case makeTimeOfDayValid
             (fromIntegral hour)
             (fromIntegral minute)
             (MkFixed (mili' * 1000000000)) of
        Nothing -> fail $ "Can't make valid time from " ++ show hour ++ ", " ++ show minute ++ ", " ++ show mili'
        Just t -> pure t
    pure (Time timeOfDay' tz)

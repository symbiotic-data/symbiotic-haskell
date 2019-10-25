{-# LANGUAGE
    DeriveGeneric
  #-}

module Data.Symbiotic.Casual.Chronological.Time where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import qualified Data.Text as T
import Data.Fixed (Fixed (..))
import GHC.Generics (Generic)


data Time = Time
  { timeOfDay :: TimeOfDay
  , timeZone  :: TimeZone
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Time where
  toJSON (Time timeOfDay'@(TimeOfDay _ _ (MkFixed pico)) timeZone') = String $
    let t = T.pack (formatTime defaultTimeLocale "%H%M" timeOfDay')
        tz = T.pack (formatTime defaultTimeLocale "%z" timeZone')
        mili :: Integer
        mili = pico `div` 1000000000
        mili' :: Float
        mili' = fromIntegral mili / 60
        pre0Mili
          | mili < 10 = "0" ++ show mili'
          | otherwise = show mili'
        shownMili
          | miliLen == 2 = pre0Mili ++ ".000"
          | miliLen < 6 = pre0Mili ++ "." ++ (replicate (5 - miliLen) '0')
          | otherwise = take 6 pre0Mili
          where
            miliLen = length pre0Mili
    in  t <> T.pack shownMili <> tz

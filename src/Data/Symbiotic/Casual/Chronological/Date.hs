{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Casual.Chronological.Date where

import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import qualified Data.Text as T
import GHC.Generics (Generic)


newtype Date = Date {getDate :: Day}
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Date where
  toJSON (Date d) = String $ T.pack $ formatTime defaultTimeLocale "%0Y%m%d" d

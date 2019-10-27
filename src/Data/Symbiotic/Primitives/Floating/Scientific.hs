{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Symbiotic.Primitives.Floating.Scientific where

import Data.Symbiotic.Primitives.UTF8Strings.String32 (getString32, makeString32)

import qualified Data.Scientific as Sci
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize (..))
import qualified Data.Text as T
import Text.Read (readMaybe)
import GHC.Generics (Generic)


newtype Scientific = Scientific {getScientific :: Sci.Scientific}
  deriving (Generic, Show, Eq, Ord, Num, Real, Fractional, RealFrac)

instance ToJSON Scientific where
  toJSON (Scientific x) = String $ T.pack $
    let c = Sci.coefficient x
        e | c == 0 = 0 -- if coefficient is 0, then the whole value is 0
          | otherwise =
            let g :: Int -- decimal places in coefficient alone
                g | c > 0 = length (show c) - 1
                  | otherwise = length (show c) - 2
            in  Sci.base10Exponent x + g
        -- coefficient shown, but without trailing zeros (exponent)
        cShownReducedExp :: String
        cShownReducedExp
          | c == 0 = "0"
          | otherwise = dropZerosFromRight (show c)
        c' :: String -- reduced coefficient
        c' | c > 0 =
             if read cShownReducedExp < (10 :: Integer)
             then cShownReducedExp
             else take 1 cShownReducedExp ++ "." ++ drop 1 cShownReducedExp
           | c == 0 = "0"
           | otherwise = dropZerosFromRight $
             if read cShownReducedExp > (-10 :: Integer)
             then cShownReducedExp
             else take 2 cShownReducedExp ++ "." ++ drop 2 cShownReducedExp
    in  c' ++ "e" ++ (if e >= 0 then "+" else "") ++ show e
    where
      dropZerosFromRight :: String -> String
      dropZerosFromRight = reverse . dropWhile (== '0') . reverse

instance FromJSON Scientific where
  parseJSON json = case json of
    String s -> case readMaybe (T.unpack s) of
      Just x -> pure (Scientific x)
      _ -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "JSONScientific" json

instance Serialize Scientific where
  put x =
    let String y = toJSON x
    in  case makeString32 y of
          Nothing -> error "Vector32 can't be made from Scientific string"
          Just z -> put z
  get = do
    s <- getString32 <$> get
    case readMaybe (T.unpack s) of
      Nothing -> fail "Scientific"
      Just x -> pure (Scientific x)

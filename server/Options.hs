{-# LANGUAGE
    OverloadedStrings
  #-}

module Options where

import Options.Applicative
  ( option, auto, Parser, long, short, metavar, showDefault, help, value, info, fullDesc, progDesc
  , ParserInfo, helper, (<**>), strOption, flag')
import Control.Applicative ((<|>))
import Data.Restricted (Restricted, Div5, rvalue, restrict)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)


websocketPort :: Parser Int
websocketPort = option auto $
  long "websocket"
    <> short 'w'
    <> metavar "WS PORT"
    <> showDefault
    <> value 3000
    <> help "Binds to port WS PORT for WebSocket tests"

zeromqPort :: Parser Int
zeromqPort = option auto $
  long "zeromq"
    <> short 'z'
    <> metavar "ZMQ PORT"
    <> showDefault
    <> value 3001
    <> help "Binds to port ZMQ PORT for ZeroMQ tests"

keyFile :: Parser FilePath
keyFile = strOption $
  long "keyFile"
    <> short 'k'
    <> metavar "KEYFILE"
    <> showDefault
    <> value "/etc/symbiotic-server/keys.json"
    <> help "File that stores public and secret keys"


genKeys :: Parser Ports
genKeys = flag' GenKeys $
  long "gen-keys"
    <> help "Generate public and secret key to store in KEYFILE"


data Ports
  = Ports
    { websocketPort' :: Int
    , zeromqPort' :: Int
    , keyFile' :: FilePath
    }
  | GenKeys


appOpts :: ParserInfo Ports
appOpts = info (parser <**> helper) $
  fullDesc
    <> progDesc "symbiote server implementation for the symbiotic-data standard."
  where
    parser :: Parser Ports
    parser = genKeys <|> (Ports <$> websocketPort <*> zeromqPort <*> keyFile)


data Keys = Keys
  { public :: Restricted Div5 ByteString
  , secret :: Restricted Div5 ByteString
  } deriving (Show)
instance ToJSON Keys where
  toJSON (Keys public secret) = object
    [ "public" .= decodeUtf8 (rvalue public)
    , "secret" .= decodeUtf8 (rvalue secret)
    ]
instance FromJSON Keys where
  parseJSON (Object o) =
    Keys
      <$> (restrict . encodeUtf8 <$> o .: "public")
      <*> (restrict . encodeUtf8 <$> o .: "secret")
  parseJSON json = typeMismatch "Keys" json

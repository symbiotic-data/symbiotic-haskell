module Main where

import Options (appOpts, Ports (..), Keys (..))
import Options.Applicative (execParser)
import System.ZMQ4 (curveKeyPair)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS


main :: IO ()
main = do
  params <- execParser appOpts
  case params of
    GenKeys -> do
      (public,secret) <- curveKeyPair
      LBS.putStrLn (encodePretty (Keys public secret))

    Ports websocketPort zeromqPort keyFile -> do

      print (websocketPort, zeromqPort, keyFile)

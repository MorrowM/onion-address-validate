module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.Text.IO qualified as T
import Onion.Address
import System.Exit
import System.IO

main :: IO ()
main = do
  input <- BS.strip <$> BS.getContents

  case validateAddress input of
    Valid -> exitSuccess
    Invalid msg -> do
      T.hPutStrLn stderr msg
      exitFailure

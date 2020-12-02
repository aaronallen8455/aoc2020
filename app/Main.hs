module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           One

main :: IO ()
main = do
  day:_ <- getArgs

  BS.interact $ case day of
    "1a" -> day1A
    "1b" -> day1B

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"

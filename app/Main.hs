module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           One
import           Two
import           Three
import           Four
import           Five
import           Six
import           Seven
import           Eight
import           Nine
import           Ten
import           Eleven
import           Twelve
import           Thirteen
import           Fourteen

main :: IO ()
main = do
  day:c:_ <- getArgs

  BS.interact $ case day <> c of
    "1a" -> day1A
    "1b" -> day1B
    "2a" -> day2A
    "2b" -> day2B
    "3a" -> day3A
    "3b" -> day3B
    "4a" -> day4A
    "4b" -> day4B
    "5a" -> day5A
    "5b" -> day5B
    "6a" -> day6A
    "6b" -> day6B
    "7a" -> day7A
    "7b" -> day7B
    "8a" -> day8A
    "8b" -> day8B
    "9a" -> day9A
    "9b" -> day9B
    "10a" -> day10A
    "10b" -> day10B
    "11a" -> day11A
    "11b" -> day11B
    "12a" -> day12A
    "12b" -> day12B
    "13a" -> day13A
    "13b" -> day13B
    "14a" -> day14A
    "14b" -> day14B

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"

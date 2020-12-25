{-# LANGUAGE BangPatterns #-}
module TwentyFive
  ( day25A
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List

day25A :: BS.ByteString -> BS.ByteString
day25A = maybe "invalid input" (BS.pack . show . solveA . map fst)
       . traverse BS.readInt . BS.lines

solveA :: [Int] -> Int
solveA [c, d] = fastExp d cLoop
  where cLoop = findLoopSize c

fastExp :: Int -> Int -> Int
fastExp _ 0 = 1
fastExp x !e
  | even e = let p = fastExp x (e `div` 2) in mo $! p * p
  | otherwise = mo $! x * fastExp x (e - 1)

mo :: Int -> Int
mo = flip mod 20201227

-- modular multiplicative inverse
inv :: Int -> Int
inv x = fastExp x 20201225

findLoopSize :: Int -> Int
findLoopSize = succ . length . takeWhile (/= 7)
             . iterate' (mo . (*) (inv 7))


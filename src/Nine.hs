{-# LANGUAGE BangPatterns #-}
module Nine
  ( day9A
  , day9B
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List

import           SymList

day9A :: BS.ByteString -> BS.ByteString
day9A = maybe "invalid input"
              (BS.pack . show)
      . (solveA . map fst <=< traverse BS.readInt . BS.lines)

day9B :: BS.ByteString -> BS.ByteString
day9B = maybe "invalid input"
              (BS.pack . show)
      . (solveB . map fst <=< traverse BS.readInt . BS.lines)

solveA :: [Int] -> Maybe Int
solveA [] = Nothing
solveA xs = go (toSL seed) rest where
  (seed, rest) = splitAt 25 xs
  go _ [] = Nothing
  go q (x:xs)
    | passes = go (tailSL q |> x) xs
    | otherwise = Just x
    where
      passes = not $ null
        [ (a, b)
        | a : as <- tails $ fromSL q
        , b <- as
        , a + b == x
        ]

solveB :: [Int] -> Maybe Int
solveB [] = Nothing
solveB input = findSeg input =<< solveA input

findSeg :: [Int] -> Int -> Maybe Int
findSeg (x:y:zs) target =
    go ([x], [y]) (x + y) zs
  where
    go sl@(f:_, _:_) !acc (x:xs)
      | acc == target = let l = fromSL sl in Just $ minimum l + maximum l
      | acc < target =
          go (sl |> x) (acc + x) xs
      | otherwise =
          go (tailSL sl) (acc - f) (x:xs)
    go _ _ _ = Nothing


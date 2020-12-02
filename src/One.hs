{-# LANGUAGE OverloadedStrings #-}
module One where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

day1A :: BS.ByteString -> BS.ByteString
day1A = maybe "invalid input" (BS.pack . show)
      . (solve1A . map fst <=< traverse BS.readInt . BS.lines)

solve1A :: [Int] -> Maybe Int
solve1A = join (go . reverse) . sort where
  go [] _ = Nothing
  go (x:xs) (y:ys)
    | added > 2020 = go xs (y:ys)
    | added < 2020 = go (x:xs) ys
    | otherwise = Just $ x * y
    where
      added = x + y

day1B :: BS.ByteString -> BS.ByteString
day1B = maybe "invalid input" (BS.pack . show)
      . (solve1B . map fst <=< traverse BS.readInt . BS.lines)

solve1B :: [Int] -> Maybe Int
solve1B xs =
  listToMaybe
    [ a * b * c
    | (a:bs) <- tails $ sort xs
    , (b:cs) <- tails $ takeWhile (< 2020 - a) bs
    , c <- takeWhile (<= 2020 - a - b) cs
    , a + b + c == 2020
    ]

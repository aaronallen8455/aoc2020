{-# LANGUAGE OverloadedStrings #-}
module Five
  ( day5A
  , day5B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List

day5A :: BS.ByteString -> BS.ByteString
day5A = BS.pack . show . maximum . map seatId . BS.lines

seatId :: BS.ByteString -> Int
seatId bs = row * 8 + col where
  row = getRow r
  col = getCol c
  (r, c) = BS.splitAt 7 bs

getRow :: BS.ByteString -> Int
getRow = (`div` 2) . BS.foldl' go 0 where
  go acc 'F' = acc * 2
  go acc 'B' = (acc + 1) * 2

getCol :: BS.ByteString -> Int
getCol = (`div` 2) . BS.foldl' go 0 where
  go acc 'L' = acc * 2
  go acc 'R' = (acc + 1) * 2

day5B :: BS.ByteString -> BS.ByteString
day5B = maybe "no answer!" (BS.pack . show . pred . fst)
      . find (\(a, b) -> a - b /= 1)
      . (zip =<< tail)
      . sort . map seatId . BS.lines

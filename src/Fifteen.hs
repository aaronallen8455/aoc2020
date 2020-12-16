{-# LANGUAGE BangPatterns #-}
module Fifteen
  ( day15A
  , day15B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM
import           Data.List

day15A :: BS.ByteString -> BS.ByteString
day15A = maybe "invalid input"
               (BS.pack . show . solve 2019 . map fst)
       . traverse BS.readInt . BS.split ','

-- slow af
day15B :: BS.ByteString -> BS.ByteString
day15B = maybe "invalid input"
               (BS.pack . show . solve 29999999 . map fst)
       . traverse BS.readInt . BS.split ','

solve :: Int -> [Int] -> Int
solve i start = unfoldr go (IM.empty, -1, start, 0) !! i
  where
    go (m, l, s:tart, !turn) =
      Just (s, (IM.insert l turn m, s, tart, turn + 1))
    go (m, l, [], !turn) =
      case m IM.!? l of
        Nothing -> Just (0, (IM.insert l turn m, 0, [], turn + 1))
        Just pt -> let x = turn - pt
                    in Just (x, (IM.insert l turn m, x, [], turn + 1))

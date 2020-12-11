module Ten
  ( day10A
  , day10B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM
import           Data.List
import qualified Data.Vector as V

day10A :: BS.ByteString -> BS.ByteString
day10A = maybe "invalid input" (BS.pack . show . solveA . map fst)
       . traverse BS.readInt . BS.lines

solveA :: [Int] -> Int
solveA xs = oneJ * threeJ where
  sorted = 0 : sort xs
  dist = zipWith (-) (tail sorted) sorted
  oneJ = length $ filter (== 1) dist
  threeJ = (+1) . length $ filter (== 3) dist

day10B :: BS.ByteString -> BS.ByteString
day10B = maybe "invalid input" (BS.pack . show . solveB . map fst)
       . traverse BS.readInt . BS.lines

solveB :: [Int] -> Int
solveB xs = numWays V.! 0 where
  m :: IM.IntMap Int
  m = IM.fromListWith (+) $ (0 : xs) `zip` repeat 1
  device = fst (IM.findMax m) + 3
  numWays = V.generate device build
  build i =
    sum [ findWays $ i + 1
        , findWays $ i + 2
        , findWays $ i + 3
        ]
  findWays i
    | i > device = 0
    | i == device = 1
    | otherwise =
        case m IM.!? i of
          Nothing -> 0
          Just x -> x * numWays V.! i


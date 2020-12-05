{-# LANGUAGE BangPatterns #-}
module Three
  ( day3A
  , day3B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable

day3A :: BS.ByteString -> BS.ByteString
day3A = BS.pack . show . numTrees 3 . BS.lines

numTrees :: Int -> [BS.ByteString] -> Int
numTrees skip = snd . foldl' go (0, 0) where
  go (!pos, !trees) bs = ((pos + skip) `mod` len, trees + t)
    where
      len = BS.length bs
      t = fromEnum $ BS.index bs pos == '#'

day3B :: BS.ByteString -> BS.ByteString
day3B = BS.pack . show . solveB . BS.lines

solveB :: [BS.ByteString] -> Int
solveB grid = product $ map (uncurry numTrees) paths where
  paths =
    [ (1, grid)
    , (3, grid)
    , (5, grid)
    , (7, grid)
    , (1, map fst . filter snd . zip grid $ cycle [True, False])
    ]


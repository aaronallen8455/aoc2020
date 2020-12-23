{-# LANGUAGE BangPatterns #-}
module TwentyThree
  ( day23A
  , day23B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM

type Cup = Int
type Ring = (Cup, IM.IntMap Cup)


handleInput :: ([Int] -> BS.ByteString) -> BS.ByteString -> BS.ByteString
handleInput f = f . map (read . (:[])) . init . BS.unpack

day23A :: BS.ByteString -> BS.ByteString
day23A = handleInput (BS.pack . concatMap show . solveA . mkRing)

-- 42 seconds
day23B :: BS.ByteString -> BS.ByteString
day23B = handleInput (BS.pack . show . product . solveB . mkRing . (++ [10..1000000]))

mkRing :: [Cup] -> Ring
mkRing (x:xs) = (,) x . IM.fromList $ zip (x:xs) (xs ++ [x])

solveA :: Ring -> [Cup]
solveA = unlink . drop 100 . iterate step where
  unlink ((_, r):_) = take 8 $ linked (1, r)

solveB :: Ring -> [Cup]
solveB = unlink . drop 10000000 . iterate step where
  unlink ((_, r):_) = take 2 $ linked (1, r)

step :: Ring -> Ring
step ring@(!c, !r) = (lT, r') where
  next3@[f,_,l] = findNext3 ring
  lT = r IM.! l
  dest = findDestination ring next3
  destT = r IM.! dest
  r' = IM.insert c lT
     . IM.insert dest f
     $ IM.insert l destT r

findDestination :: Ring -> [Cup] -> Cup
findDestination (c, r) next3 = go c where
  go x =
    case IM.lookupLT x r of
      Just (k, _)
        | k `elem` next3 -> go k
        | otherwise -> k
      Nothing -> go 1000001

findNext3 :: Ring -> [Cup]
findNext3 = take 3 . linked

linked :: Ring -> [Cup]
linked (c, r) = go c where
  go x = let y = r IM.! x in y : go y

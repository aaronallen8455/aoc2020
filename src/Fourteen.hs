module Fourteen
  ( day14A
  , day14B
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import           Data.List

day14A :: BS.ByteString -> BS.ByteString
day14A = maybe "invalid input" (BS.pack . show . solveA)
       . traverse parseLine . BS.lines

day14B :: BS.ByteString -> BS.ByteString
day14B = maybe "invalid input" (BS.pack . show . solveBTree)
       . traverse parseLine . BS.lines

data Line
  = Mask String
  | Write Int Int
  deriving Show

parseLine :: BS.ByteString -> Maybe Line
parseLine bs = do
  let [instr, val] = BS.split '=' bs
  case BS.splitAt 4 instr of
    ("mask", _) -> pure . Mask . BS.unpack $ BS.tail val
    ("mem[", nb) -> do
      (n, _) <- BS.readInt nb
      (v, _) <- BS.readInt $ BS.tail val
      pure $ Write n v

solveA :: [Line] -> Int
solveA = sum . fst . foldl' go (IM.empty, "") where
  go (mem, _) (Mask m) = (mem, m)
  go (mem, mask) (Write n v) = (IM.insert n r mem, mask) where
    r = applyMaskA mask v

applyMaskA :: String -> Int -> Int
applyMaskA mask v = foldl' go v ([35, 34 ..] `zip` mask) where
  go x (_, 'X') = x
  go x (i, '1') = setBit x i
  go x (i, '0') = clearBit x i

solveB :: [Line] -> Int
solveB = sum . fst . foldl' go (IM.empty, "") where
  go (mem, _) (Mask m) = (mem, m)
  go (mem, mask) (Write n v) = (newMem, mask) where
    newMem = foldl' insert mem $ traverse floatBits mask
    floatBits 'X' = ['1', 'X']
    floatBits x = [x]
    insert mem mask = IM.insert (applyMaskB mask n) v mem

applyMaskB :: String -> Int -> Int
applyMaskB mask v = foldl' go v ([35, 34 ..] `zip` mask) where
  go x (i, '1') = setBit x i
  go x (_, '0') = x
  go x (i, 'X') = clearBit x i

-- better performance using a binary tree instead of IntMap
data Tree a = Nil | Leaf !a | DoubleNode !(Tree a) | Node !(Tree a) !(Tree a)

mapLeft :: (Tree a -> Tree a) -> Tree a -> Tree a
mapLeft f Nil = Node (f Nil) Nil
mapLeft f (Node l r) = Node (f l) r
mapLeft f (DoubleNode t) = Node (f t) t

mapRight :: (Tree a -> Tree a) -> Tree a -> Tree a
mapRight f Nil = Node Nil (f Nil)
mapRight f (Node l r) = Node l (f r)
mapRight f (DoubleNode t) = Node t (f t)

mapBoth :: (Tree a -> Tree a) -> Tree a -> Tree a
mapBoth f Nil = DoubleNode (f Nil)
mapBoth f (DoubleNode t) = DoubleNode (f t)
mapBoth f t = mapLeft f $ mapRight f t

solveBTree :: [Line] -> Int
solveBTree = sumTree . fst . foldl' go (Nil, "") where
  go (mem, _) (Mask m) = (mem, reverse m)
  go (mem, mask) (Write n v) = (insertTree mask n v mem, mask)

insertTree :: String -> Int -> Int -> Tree Int -> Tree Int
insertTree [] _ v _ = Leaf v
insertTree ('0':mask) k v t
  | m == 0 = mapLeft (insertTree mask d v) t
  | otherwise = mapRight (insertTree mask d v) t
  where
    (d, m) = divMod k 2
insertTree ('1':mask) k v t =
  mapRight (insertTree mask (div k 2) v) t
insertTree ('X':mask) k v t =
  mapBoth (insertTree mask (div k 2) v) t

sumTree :: Tree Int -> Int
sumTree (DoubleNode t) = 2 * sumTree t
sumTree (Leaf x) = x
sumTree (Node l r) = sumTree l + sumTree r
sumTree Nil = 0

module Fourteen
  ( day14A
  , day14B
  ) where

import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import           Data.List

day14A :: BS.ByteString -> BS.ByteString
day14A = maybe "invalid input" (BS.pack . show . solveA)
       . traverse parseLine . BS.lines

day14B :: BS.ByteString -> BS.ByteString
day14B = maybe "invalid input" (BS.pack . show . solveB)
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

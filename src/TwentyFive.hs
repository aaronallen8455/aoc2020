{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TwentyFive
  ( day25A
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import qualified Data.IntMap as IM

day25A :: BS.ByteString -> BS.ByteString
day25A = maybe "invalid input" (BS.pack . show . solveA . map fst)
       . traverse BS.readInt . BS.lines

solveA :: [Int] -> Int
solveA [c, d] = unModExp $ stimes cLoop (ModExp d)
  where cLoop = discreteLog 7 c

newtype ModExp = ModExp { unModExp :: Int }
  deriving (Show, Num)

instance Semigroup ModExp where
  ModExp a <> ModExp b = ModExp . mo $! a * b

mo :: Int -> Int
mo = flip mod m

m :: Int
m = 20201227

-- giant step baby step algorithm
-- (https://www.geeksforgeeks.org/discrete-logarithm-find-integer-k-ak-congruent-modulo-b/)
discreteLog :: Int -> Int -> Int
discreteLog a b = head rhs where
  n = ceiling (sqrt $ fromIntegral m)
  an = stimes n (ModExp a)
  lhs = IM.fromList . reverse . take n . (`zip` [1..]) . fmap unModExp
      $ iterate' (<> an) an
  rhs = mapMaybe match . zip [0..] $ iterate' (<> ModExp a) (ModExp b)
  match (i, ModExp x) = do
    y <- lhs IM.!? x
    let res = y * n - i
    guard $ res < m
    pure res


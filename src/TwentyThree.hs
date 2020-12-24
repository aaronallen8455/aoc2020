{-# LANGUAGE BangPatterns #-}
module TwentyThree
  ( day23A
  , day23B
  ) where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import           Data.List
import qualified Data.STRef as ST
import qualified Data.Vector.Unboxed.Mutable as V

type Cup = Int
type Ring = (Cup, IM.IntMap Cup)
type RingM s = V.MVector s Cup

handleInput :: ([Cup] -> BS.ByteString) -> BS.ByteString -> BS.ByteString
handleInput f = f . map (read . (:[])) . init . BS.unpack

day23A :: BS.ByteString -> BS.ByteString
day23A = handleInput (BS.pack . concatMap show . solveA . mkRing)

-- 42 seconds
day23Bpure :: BS.ByteString -> BS.ByteString
day23Bpure = handleInput (BS.pack . show . product . solveB . mkRing . (++ [10..1000000]))

-- using unboxed mutable vector - 1.2 seconds!
day23B :: BS.ByteString -> BS.ByteString
day23B = handleInput $ \i -> BS.pack . show $ runST (solveBM =<< mkRingM i)

mkRing :: [Cup] -> Ring
mkRing (x:xs) = (,) x . IM.fromList $ zip (x:xs) (xs ++ [x])

solveA :: Ring -> [Cup]
solveA = unlink . drop 100 . iterate' step where
  unlink ((_, r):_) = take 8 $ linked (1, r)

solveB :: Ring -> [Cup]
solveB = unlink . drop 10000000 . iterate' step where
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

mkRingM :: [Cup] -> ST s (RingM s)
mkRingM cs = do
  let cs' = 1000000 : cs ++ [10]
      cs'' = zip cs' $ tail cs'
  q <- ST.newSTRef $ head cs : [2..1000000] <> [1]
  v <- V.replicateM 1000001 $ do
    Just (x, xs) <- uncons <$> ST.readSTRef q
    ST.writeSTRef q xs
    pure x
  forM_ cs'' $ \(s, t) -> do
    V.write v s t
  pure v

solveBM :: RingM s -> ST s Int
solveBM r = replicateM_ 10000000 (stepM r) >> getAnswer r where
  getAnswer r = do
    a <- V.read r 1
    b <- V.read r a
    pure $ a * b

stepM :: RingM s -> ST s ()
stepM !r = do
  c <- V.read r 0
  nxt <- V.read r c
  nxt' <- V.read r nxt
  nxt'' <- V.read r nxt'

  let dest = until good (wrapAround . subtract 1) c
      wrapAround 0 = 1000000
      wrapAround n = n
      good x = x `notElem` [c, nxt, nxt', nxt'']

  cT <- V.read r nxt''
  V.write r c cT
  dT <- V.read r dest
  V.write r dest nxt
  V.write r nxt'' dT
  nc <- V.read r c
  V.write r 0 nc

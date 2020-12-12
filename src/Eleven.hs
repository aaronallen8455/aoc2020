{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Eleven where

import           Control.Arrow
import           Control.Comonad
import           Control.Monad
import           Data.Array
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid (Sum(..))

type Boat = Array (Int, Int) Char

day11A :: BS.ByteString -> BS.ByteString
day11A = BS.pack . show
       . solve stepA
       . buildMap
       . BS.lines

day11B :: BS.ByteString -> BS.ByteString
day11B = BS.pack . show
       . solve stepB
       . buildMap
       . BS.lines

buildMap :: [BS.ByteString] -> Boat
buildMap bs = listArray ((0, 0), (h, w)) $ concatMap BS.unpack bs
  where
    h = length bs - 1
    w = pred . BS.length $ head bs

solve :: (Grid Char -> Char) -> Boat -> Int
solve step = go . map grid
           . iterate (extend step) . Grid (0, 0)
  where
    go (x:y:zs)
      | x == y = numOccupied x
      | otherwise = go $ y:zs

data Grid a =
  Grid { focus :: (Int, Int)
       , grid :: Array (Int, Int) a
       } deriving Functor

instance Comonad Grid where
  extract = (!) <$> grid <*> focus
  duplicate g = g { grid = duped } where
    duped = listArray (bounds $ grid g) $ do
      b <- indices $ grid g
      pure $ g { focus = b }

stepA :: Grid Char -> Char
stepA gr@(Grid (y, x) g) = newC . length $ filter (== '#') adj
  where
    (_, (mh, mw)) = bounds g
    adj = do
      (x', y') <- (,) <$> [x-1..x+1] <*> [y-1..y+1]

      guard $ (x /= x' || y /= y') && isValid g (y', x')

      pure $ g ! (y', x')
    newC n
      | c == 'L'
      , n == 0
      = '#'
      | c == '#'
      , n >= 4
      = 'L'
      | otherwise = c
    c = extract gr

numOccupied :: Boat -> Int
numOccupied = getSum . foldMap (\case { '#' -> 1; _ -> 0 })

inSight :: Boat -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Char
inSight m nxt = foldr go '.' . takeWhile (isValid m) . iterate nxt . nxt
  where
    go i acc
      | c == '.' =  acc
      | otherwise = c
      where c = m ! i

isValid :: Boat -> (Int, Int) -> Bool
isValid m (y, x) = x >= 0 && y >= 0 && x <= mx && y <= my
  where
    (_, (my, mx)) = bounds m

stepB :: Grid Char -> Char
stepB gr@(Grid coord g) = newC . length $ filter (== '#') adj
  where
    (_, (mh, mw)) = bounds g
    adj = [ inSight g (first succ) coord
          , inSight g (first pred) coord
          , inSight g (second succ) coord
          , inSight g (second pred) coord
          , inSight g (succ *** succ) coord
          , inSight g (succ *** pred) coord
          , inSight g (pred *** succ) coord
          , inSight g (pred *** pred) coord
          ]
    newC n
      | c == 'L'
      , n == 0
      = '#'
      | c == '#'
      , n >= 5
      = 'L'
      | otherwise = c
    c = extract gr


{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Seventeen
  ( day17A
  , day17B
  ) where

import           Control.Comonad
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

type Coord3 = (Int, Int, Int)
type Coord4 = (Int, Int, Int, Int)

data GridZip k a =
  G { focus :: k
    , grid :: M.Map k a
    , def :: a
    } deriving Functor

instance (Adjacent k, Ord k) => Comonad (GridZip k) where
  extract = M.findWithDefault <$> def <*> focus <*> grid
  duplicate gz@G{focus=f, grid=g, def=d} = G f newG gz
    where
      newG = newCells <> M.mapWithKey (\k x -> G k g d) g
      newCells = M.fromAscList . map (\k -> (k, G k g d)) $ S.toList coords
      curCoords = M.keysSet g
      coords = S.fromList $ do
        c <- M.keys g
        c' <- adj c
        guard $ S.notMember c' curCoords
        pure c'

class Adjacent a where
  adj :: a -> [a]

instance Adjacent Coord3 where
  adj f@(x, y, z) = do
    f' <- (,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1]
    guard $ f /= f'
    pure f'

instance Adjacent Coord4 where
  adj f@(x, y, z, w) = do
    f' <- (,,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> [w-1..w+1]
    guard $ f /= f'
    pure f'

day17A :: BS.ByteString -> BS.ByteString
day17A = handleInput ((0,0,0) :: Coord3) (\x y -> (x, y, 0))

day17B :: BS.ByteString -> BS.ByteString
day17B = handleInput ((0,0,0,0) :: Coord4) (\x y -> (x, y, 0, 0))

handleInput :: (Ord k, Adjacent k) => k -> (Int -> Int -> k) -> BS.ByteString -> BS.ByteString
handleInput k f = BS.pack . show . solve . mkZ . foldMap mkG . zip [0..] . BS.lines
  where
    mkG (y, bs) = foldMap fromCell $ [0..] `zip` BS.unpack bs where
      fromCell (x, c) = M.singleton (f x y) (c == '#')
    mkZ m = G k m False

solve :: (Ord k, Adjacent k) => GridZip k Bool -> Int
solve = total . (!! 6) . iterate (extend step)
  where
    total = sum . fmap fromEnum . grid

step :: (Ord k, Adjacent k) => GridZip k Bool -> Bool
step gz@G{focus=f, grid=g}
  | fc
  , numActive == 2 || numActive == 3
  = True
  | not fc
  , numActive == 3
  = True
  | otherwise = False
  where
    fc = extract gz
    numActive = length . filter id $ do
      c <- adj f
      pure $ M.findWithDefault False c g


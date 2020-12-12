{-# LANGUAGE LambdaCase #-}
module Twelve
  ( day12A
  , day12B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List

day12A :: BS.ByteString -> BS.ByteString
day12A = maybe "invalid input" (BS.pack . show . solveA)
       . traverse parseInstr . BS.lines

data Instr
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving Show

data Dir
  = North
  | South
  | East
  | West

parseInstr :: BS.ByteString -> Maybe Instr
parseInstr bs = do
  (c, (n, _)) <- traverse BS.readInt $ BS.splitAt 1 bs
  pure $ case c of
    "N" -> N n
    "S" -> S n
    "E" -> E n
    "W" -> W n
    "L" -> L n
    "R" -> R n
    "F" -> F n

solveA :: [Instr] -> Int
solveA = (\(a, b) -> abs a + abs b) . snd . foldl' applyInstr (East, (0, 0))

left90 :: Dir -> Dir
left90 North = West
left90 South = East
left90 East = North
left90 West = South

applyInstr :: (Dir, (Int, Int)) -> Instr -> (Dir, (Int, Int))
applyInstr (facing, (x, y)) = \case
  N n -> (facing, (x, y + n))
  S n -> (facing, (x, y - n))
  E n -> (facing, (x + n, y))
  W n -> (facing, (x - n, y))
  F n -> applyInstr (facing, (x,y)) $
           case facing of { North -> N n; South -> S n; East -> E n; West -> W n }
  L 90 -> (left90 facing, (x,y))
  L 180 -> (left90 $ left90 facing, (x,y))
  L 270 -> (left90 . left90 $ left90 facing, (x,y))
  R 90 -> (left90 . left90 $ left90 facing, (x,y))
  R 180 -> (left90 $ left90 facing, (x,y))
  R 270 -> (left90 facing, (x,y))
  x -> error (show x)

day12B :: BS.ByteString -> BS.ByteString
day12B = maybe "invalid input" (BS.pack . show . solveB)
       . traverse parseInstr . BS.lines

solveB :: [Instr] -> Int
solveB = (\(a, b) -> abs a + abs b) . snd . foldl' applyInstrB ((10, 1), (0, 0)) where
  applyInstrB (wp@(wx, wy), shp@(sx, sy)) = \case
    N n -> ((wx, wy + n), shp)
    S n -> ((wx, wy - n), shp)
    E n -> ((wx + n, wy), shp)
    W n -> ((wx - n, wy), shp)
    F n -> (wp, (sx + n * wx, sy + n * wy))
    L 90 -> ((-wy, wx), shp)
    L 180 -> ((-wx, -wy), shp)
    L 270 -> ((wy, -wx), shp)
    R 90 -> ((wy, -wx), shp)
    R 180 -> ((-wx, -wy), shp)
    R 270 -> ((-wy, wx), shp)
    x -> error (show x)


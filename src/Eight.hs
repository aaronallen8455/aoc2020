{-# LANGUAGE BangPatterns #-}
module Eight
  ( day8A
  , day8B
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

day8A :: BS.ByteString -> BS.ByteString
day8A = maybe "invalid input"
              (BS.pack . show . solveA IS.empty 0 0 . V.fromList)
      . traverse parseLine . BS.lines

day8B :: BS.ByteString -> BS.ByteString
day8B = maybe "invalid input" (BS.pack . show)
      . ((`evalStateT` IM.empty) . solveB False 0 0 . V.fromList
          <=< traverse parseLine . BS.lines
        )

data Cmd
  = NoOp Int
  | Jump Int
  | Acc Int

parseLine :: BS.ByteString -> Maybe Cmd
parseLine bs =
  case BS.splitAt 4 bs of
    ("nop ", rest) -> NoOp <$> parseNum rest
    ("acc ", rest) -> Acc <$> parseNum rest
    ("jmp ", rest) -> Jump <$> parseNum rest
  where
    parseNum r = fmap fst . BS.readInt $ BS.dropWhile (== '+') r

solveA :: IS.IntSet -> Int -> Int -> V.Vector Cmd -> Int
solveA visited !ix !acc v
  | IS.member ix visited = acc
  | otherwise =
    case v V.! ix of
      NoOp _ -> solveA visited' (ix + 1) acc v
      Jump n -> solveA visited' (ix + n) acc v
      Acc n -> solveA visited' (ix + 1) (acc + n) v
  where
    visited' = IS.insert ix visited

solveB :: Bool -> Int -> Int -> V.Vector Cmd
       -> StateT (IM.IntMap Bool) Maybe Int
solveB post !ix !acc v
  | ix == V.length v = pure acc
  | otherwise = do
    mbVisited <- gets $ IM.lookup ix
    case mbVisited of
      Just True -> empty
      Just False | not post -> empty
      _ -> do
        modify' $ IM.insert ix post
        case v V.! ix of
          NoOp n
            | post -> solveB post (ix + 1) acc v
            | otherwise -> solveB True (ix + n) acc v
                       <|> solveB post (ix + 1) acc v
          Jump n
            | post -> solveB post (ix + n) acc v
            | otherwise -> solveB True (ix + 1) acc v
                       <|> solveB post (ix + n) acc v
          Acc n -> solveB post (ix + 1) (acc + n) v


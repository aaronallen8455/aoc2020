{-# LANGUAGE BangPatterns #-}
module TwentyTwo
  ( day22A
  , day22B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Bitraversable
import qualified Data.Set as S

import           SymList

type Deck = SymList Int

handleInput :: ((Deck, Deck) -> Int) -> BS.ByteString -> BS.ByteString
handleInput f
  = maybe "invalid input" (BS.pack . show . f)
  . bitraverse parseDeck (parseDeck . tail) . span (/= "") . BS.lines

day22A :: BS.ByteString -> BS.ByteString
day22A = handleInput solveA

day22B :: BS.ByteString -> BS.ByteString
day22B = handleInput solveB

parseDeck :: [BS.ByteString] -> Maybe (SymList Int)
parseDeck = fmap (toSL . map fst) . traverse BS.readInt . tail

solveA :: (Deck, Deck) -> Int
solveA (a, b) =
  case (unconsSL a, unconsSL b) of
    (_, Nothing) -> score a
    (Just (x, xs), Just (y, ys))
      | x > y -> solveA (xs |> x |> y, ys)
      | otherwise -> solveA (ys |> y |> x, xs)
    _ -> error "impossible"

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . fromSL

solveB :: (Deck, Deck) -> Int
solveB decks@(ad, bd) = either score score $
    go S.empty (lengthSL ad - 1) (lengthSL bd - 1) decks
  where
    go mem !al !bl decks@(ad, bd)
      | decks `S.member` mem = Left ad
      | otherwise =
          case (unconsSL ad, unconsSL bd) of
            (_, Nothing) -> Left ad
            (Nothing, _) -> Right bd
            (Just (a, ad'), Just (b, bd'))
              | a <= al && b <= bl
              -> case go S.empty (a-1) (b-1) (takeSL a ad', takeSL b bd') of
                   Left _ -> aWon
                   Right _ -> bWon
              | a > b -> aWon
              | otherwise -> bWon
              where
                aWon = go mem' (al + 1) (bl - 1) (ad' |> a |> b, bd')
                bWon = go mem' (al - 1) (bl + 1) (ad', bd' |> b |> a)
                mem' = S.insert decks mem

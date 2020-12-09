module One where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

day1A :: BS.ByteString -> BS.ByteString
day1A = maybe "invalid input" (BS.pack . show)
      . (solve1A . map fst <=< traverse BS.readInt . BS.lines)

solve1A :: [Int] -> Maybe Int
solve1A = join (go . reverse) . sort where
  go (x:xs) (y:ys)
    | added > 2020 = go xs (y:ys)
    | added < 2020 = go (x:xs) ys
    | otherwise = Just $ x * y
    where
      added = x + y
  go _ _ = Nothing

day1B :: BS.ByteString -> BS.ByteString
day1B = maybe "invalid input" (BS.pack . show)
      . (fancy . map fst <=< traverse BS.readInt . BS.lines)

solve1B :: [Int] -> Maybe Int
solve1B xs =
  listToMaybe
    [ a * b * c
    | (a:bs) <- tails $ sort xs
    , (b:cs) <- tails $ takeWhile (< 2020 - a) bs
    , c <- takeWhile (<= 2020 - a - b) cs
    , a + b + c == 2020
    ]

fancy :: [Int] -> Maybe Int
fancy xs = go 0 1 (len - 1) `evalStateT` S.empty
  where
    vec = V.fromList $ sort xs
    len = V.length vec
    go xi yi zi = do
      guard $ xi < yi && yi < zi && zi < len
      visited <- get
      guard $ (xi, yi, zi) `S.notMember` visited
      modify (S.insert (xi, yi, zi))
      case x + y + z of
        s | s < 2020
               -> go (xi + 1) yi zi
              <|> go xi (yi + 1) zi
          | s > 2020
               -> go xi yi (zi - 1)
              <|> go xi (yi - 1) zi
          | otherwise -> pure $ x * y * z
      where
        x = vec V.! xi
        y = vec V.! yi
        z = vec V.! zi


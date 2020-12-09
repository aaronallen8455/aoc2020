module Seven
  ( day7A
  , day7B
  ) where

import           Control.Monad
import           Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Maybe

day7A :: BS.ByteString -> BS.ByteString
day7A = BS.pack . show . S.size . S.fromList . flip getResultsA "shiny gold"
      . M.fromListWith (<>) . revDeps
      . map parseLine . BS.lines

day7B :: BS.ByteString -> BS.ByteString
day7B = BS.pack . show . flip getResultsB "shiny gold"
      . M.fromListWith (<>)
      . map parseLine . BS.lines

parseLine :: BS.ByteString -> (BS.ByteString, [(Int, BS.ByteString)])
parseLine bs = (bagName, subBags) where
  (bagName, rest) = BS.breakSubstring " bags contain" bs
  rest' = BS.drop 14 rest
  subBags = mapMaybe parseSubBag
              $ BS.split ',' rest'

parseSubBag :: BS.ByteString -> Maybe (Int, BS.ByteString)
parseSubBag bs = fmap (BS.unwords . init . BS.words)
             <$> BS.readInt (BS.dropWhile (== ' ') bs)

revDeps :: [(BS.ByteString, [(Int, BS.ByteString)])]
        -> [(BS.ByteString, [BS.ByteString])]
revDeps deps = do
  d <- deps
  (_, sd) <- snd d
  pure (sd, [fst d])

getResultsA :: M.Map BS.ByteString [BS.ByteString]
            -> BS.ByteString
            -> [BS.ByteString]
getResultsA m s =
  case M.lookup s m of
    Nothing -> []
    Just sbs -> sbs <> concatMap (getResultsA m) sbs

getResultsB :: M.Map BS.ByteString [(Int, BS.ByteString)]
            -> BS.ByteString
            -> Int
getResultsB m b =
  case M.lookup b m of
    Nothing -> 0
    Just sbs -> sum $ do
      sb <- sbs
      let r = getResultsB m (snd sb) + 1
      pure $! fst sb * r

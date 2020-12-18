module Sixteen
  ( day16A
  , day16B
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Vector as V

data Rule =
  Rule
    { range1 :: Range
    , range2 :: Range
    , name :: BS.ByteString
    }

type Range = (Int, Int)
type Ticket = [Int]

day16A :: BS.ByteString -> BS.ByteString
day16A inp = fromMaybe "invalid input" $ do
  let [rs, t, ts] = filter (/= [""]) . groupBy (\a b -> a /= "" && b /= "") $ BS.lines inp
  rules <- traverse parseRule rs
  _ticket <- parseTicket $ last t
  tickets <- traverse parseTicket $ tail ts
  let r = concatMap (filter (not . applyRules rules)) tickets
  pure . BS.pack . show $ sum r

day16B :: BS.ByteString -> BS.ByteString
day16B inp = fromMaybe "invalid input" $ do
  let [rs, t, ts] = filter (/= [""]) . groupBy (\a b -> a /= "" && b /= "") $ BS.lines inp
  rules <- traverse parseRule rs
  myTicket <- parseTicket $ last t
  tickets <- traverse parseTicket $ tail ts
  let filteredTicks = filter (all (applyRules rules)) tickets
      namedTicket = myTicket `zip` assignFields rules filteredTicks
      r = product . map fst $ filter (BS.isPrefixOf "departure" . snd) namedTicket
  pure . BS.pack $ show r

parseRule :: BS.ByteString -> Maybe Rule
parseRule bs = do
  let [tag, rest] = BS.split ':' bs
  (r1, b) <- parseRange $ BS.tail rest
  (r2, _) <- parseRange $ BS.drop 4 b
  pure $ Rule r1 r2 tag

parseRange :: BS.ByteString -> Maybe ((Int, Int), BS.ByteString)
parseRange bs = do
  (a, r) <- BS.readInt bs
  (b, r2) <- BS.readInt $ BS.tail r
  pure ((a, b), r2)

parseTicket :: BS.ByteString -> Maybe Ticket
parseTicket = fmap (map fst) . traverse BS.readInt . BS.split ','

applyRules :: [Rule] -> Int -> Bool
applyRules rs = getAny . foldMap ((Any .) . applyRule) rs

applyRule :: Rule -> Int -> Bool
applyRule r x = inRange (range1 r) x || inRange (range2 r) x
  where
    inRange (l, u) x = x >= l && x <= u

assignFields :: [Rule] -> [Ticket] -> [BS.ByteString]
assignFields rules tickets = findMatching . map (map name)
                           $ foldr go rules <$> transpose tickets
  where
    go x = filter (`applyRule` x)

findMatching :: [[BS.ByteString]] -> [BS.ByteString]
findMatching groups = maybe [] (map fst . sortOn snd . M.toList)
                    $ foldM go M.empty [0 .. V.length v - 1]
  where
  v = V.fromList groups
  go m vi = msum [ assign S.empty vi c m | c <- cands] where
    cands = v V.! vi
    assign visited i k m
      | S.member k visited = Nothing
      | Just ci <- m M.!? k
      , let possibles = v V.! ci
            m' = M.insert k i m
            visited' = S.insert k visited
      = msum [ assign visited' ci p m' | p <- possibles ]
      | otherwise = Just $ M.insert k i m


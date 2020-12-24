{-# LANGUAGE DeriveFunctor #-}
module TwentyFour where

import           Control.Comonad
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Text.Parsec

type Parser = Parsec BS.ByteString ()
type Path = (Int, Int)
type Floor a = M.Map Path a

data TileZip a =
  TileZip
    { focus :: Path
    , flo :: Floor a
    , conv :: Bool -> a
    } deriving Functor

instance Comonad TileZip where
  extract z = M.findWithDefault (conv z False) (focus z) (flo z)
  duplicate z = z { flo = newFlo, conv = const z } where
    curPaths = M.keysSet (flo z)
    newFlo = fringe <> M.mapWithKey (\k _ -> z { focus = k }) (flo z)
    fringe = M.fromList newPaths
    newPaths = do
      p <- S.toList curPaths
      c <- adj p
      guard $ c `S.notMember` curPaths
      pure (c, z { focus = c })

adj :: Path -> [Path]
adj (x, y) = [ (x+1, y-1), (x-1, y+1), (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

handleInput :: ([Path] -> Int) -> BS.ByteString -> BS.ByteString
handleInput f = either (BS.pack . show) (BS.pack . show . f)
              . traverse (parse (parsePath <* eof) "")
              . BS.lines

day24A :: BS.ByteString -> BS.ByteString
day24A = handleInput solveA
  where
    solveA = length . filter odd . M.elems
           . M.fromListWith (+) . flip zip (repeat 1)

day24B :: BS.ByteString -> BS.ByteString
day24B = handleInput solveB where
  solveB ps = numBlack . (!! 100) $ iterate' (extend step) z where
    ps' = map fst . filter (odd . snd) . M.toList . M.fromListWith (+)
        $ ps `zip` repeat 1
    fl = M.fromList $ ps' `zip` repeat True
    z = TileZip (0, 0) fl id
    numBlack = length . filter id . M.elems . flo

parsePath :: Parser Path
parsePath = do
  foldr1 (\(a,b) (c, d) -> (a+c, b+d)) <$> many1 parseDir

parseDir :: Parser (Int, Int)
parseDir = (1, -1) <$ try (string "se")
       <|> (-1, 1) <$ try (string "nw")
       <|> (1, 0) <$ char 'e'
       <|> (-1, 0) <$ char 'w'
       <|> (0, 1) <$ string "ne"
       <|> (0, -1) <$ try (string "sw")

step :: TileZip Bool -> Bool
step z = newV where
  f = extract z
  adjBlack = length . filter id $ do
    c <- adj $ focus z
    pure $ M.findWithDefault False c (flo z)
  newV = if f
            then not $ adjBlack == 0 || adjBlack > 2
            else adjBlack == 2

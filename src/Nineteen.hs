module Nineteen
  ( day19A
  , day19B
  ) where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import qualified Data.IntMap.Lazy as IM
import           Text.Parsec

type Parser = Parsec BS.ByteString ()
type ParC = Parser () -> Parser ()

day19A, day19B :: BS.ByteString -> BS.ByteString
day19A = handleInput False
day19B = handleInput True

handleInput :: Bool -> BS.ByteString -> BS.ByteString
handleInput sub = either BS.pack (BS.pack . show) . solve sub . break (== "") . BS.lines

solve :: Bool -> ([BS.ByteString], [BS.ByteString]) -> Either String Int
solve sub (bsRules, _:msgs) = do
  pairs <- first show
         $ traverse (parse parseParser "rules") bsRules

  let pMap = IM.fromList $ sequence pairs pMap
                         ++ if sub
                               then
                                 [ (8, replace8 pMap)
                                 , (11, replace11 pMap)
                                 ]
                               else []

  p <- maybe (Left "no rule 0") Right $ pMap IM.!? 0
  pure . length . rights $ map (parse (p eof) "msgs") msgs

parseParser :: Parser (IM.IntMap ParC -> (Int, ParC))
parseParser = do
  i <- read <$> many1 digit
  _ <- string ": "
  let pRefs :: Parser (Parser () -> IM.IntMap ParC -> Parser ())
      pRefs = do
        ps <- many1 (pRef <* spaces)
        let go x acc m =
              x (acc m) m
        pure $ \k -> foldr go (const k) ps
      pRef = do
        ix <- read <$> many1 digit
        pure $ \k m ->
          m IM.! ix $ k
      pChar = do
        c <- char '"' *> (anyChar <* char '"')
        pure $ \k _ -> char c *> k

  rules <- sepBy1 (try pRefs <|> pChar) ((spaces *> char '|') <* spaces)

  pure $ \m -> (i, \k -> msum (map (try . ($ (k, m)) . uncurry) rules))

replace8 :: IM.IntMap ParC -> ParC
replace8 m k = do
  let p = m IM.! 42
  p $ try (replace8 m k) <|> k

replace11 :: IM.IntMap ParC -> ParC
replace11 m k = do
  let p1 = m IM.! 42
      p2 = m IM.! 31
      k' = p2 k

  p1 $ try (replace11 m k') <|> k'

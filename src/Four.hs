module Four
  ( day4A
  , day4B
  ) where

import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Byte

type Parser = Parsec () BS.ByteString
type Card = M.Map BS.ByteString BS.ByteString

day4A :: BS.ByteString -> BS.ByteString
day4A = maybe "invalid input" (BS8.pack . show . length . filter completeCard)
      . parseMaybe (many parseCard)

completeCard :: Card -> Bool
completeCard card = required `S.isSubsetOf` M.keysSet card

required :: S.Set BS.ByteString
required = S.fromList
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  -- , "cid"
  ]

parseCard :: Parser Card
parseCard = do
  fields <- some $ parseField <* (try (void spaceChar) <|> void eol)
  _ <- eol
  pure $ M.fromList fields

parseField :: Parser (BS.ByteString, BS.ByteString)
parseField = do
  key <- some letterChar
  _ <- string ":"
  val <- takeWhile1P Nothing (not . isSpace . chr . fromIntegral)
  pure (BS.pack key, val)

day4B :: BS.ByteString -> BS.ByteString
day4B = maybe "invalid input" (BS8.pack . show . length . filter validCard)
      . parseMaybe (many parseCard)

validCard :: Card -> Bool
validCard card
  | Just byr <- card M.!? "byr"
  , Just (byri, r1) <- BS8.readInt byr, BS.null r1
  , byri >= 1920, byri <= 2002

  , Just iyr <- card M.!? "iyr"
  , Just (iyri, r2) <- BS8.readInt iyr, BS.null r2
  , iyri >= 2010, iyri <= 2020

  , Just eyr <- card M.!? "eyr"
  , Just (eyri, r3) <- BS8.readInt eyr, BS.null r3
  , eyri >= 2020, eyri <= 2030

  , Just hgt <- card M.!? "hgt"
  , Just (hgti, r4) <- BS8.readInt hgt
  , (r4 == "cm" && hgti >= 150 && hgti <= 193)
    || (r4 == "in" && hgti >= 59 && hgti <= 76)

  , Just hcl <- card M.!? "hcl"
  , Just (h, cl) <- BS8.uncons hcl
  , h == '#'
  , Just _ <- parseMaybe (replicateM_ 6 hexDigitChar >> eof :: Parser ()) cl

  , Just ecl <- card M.!? "ecl"
  , ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

  , Just pid <- card M.!? "pid"
  , Just _ <- parseMaybe (replicateM_ 9 digitChar >> eof :: Parser ()) pid
  = True
  | otherwise = False


{-# LANGUAGE LambdaCase #-}
module Eighteen
  ( day18A
  , day18B
  ) where

import           Control.Applicative hiding ((<|>))
import qualified Data.ByteString.Char8 as BS
import           Text.Parsec

type Parser = Parsec BS.ByteString ()

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Par Expr
  | Id Int
  deriving Show

day18A :: BS.ByteString -> BS.ByteString
day18A = handleInput parseOp

day18B :: BS.ByteString -> BS.ByteString
day18B = handleInput parseOpB

handleInput :: Parser Expr -> BS.ByteString -> BS.ByteString
handleInput opParser
  = either (BS.pack . show) (BS.pack . show . sum . map evalExpr)
  . traverse (parse (parseExpr opParser) "")
  . BS.lines

parseExpr :: Parser Expr -> Parser Expr
parseExpr opParser = try opParser
                 <|> try (parsePar opParser)
                 <|> Id <$> parseInt

parseInt :: Parser Int
parseInt =
  (read <$> some digit) <* spaces

parsePar :: Parser Expr -> Parser Expr
parsePar opParser =
  between (char '(' <* spaces) (char ')' <* spaces)
          (parseExpr opParser)

parseOp :: Parser Expr
parseOp = chainl1 p (op <* spaces) where
  op = Add <$ char '+'
   <|> Mul <$ char '*'
  p = try (parsePar parseOp) <|> Id <$> parseInt

parseOpB :: Parser Expr
parseOpB = chainl1 add (Mul <$ char '*' <* spaces) where
  add = chainl1 p (Add <$ char '+' <* spaces)
  p = try (parsePar parseOpB) <|> Id <$> parseInt

evalExpr :: Expr -> Int
evalExpr = \case
  Id x -> x
  Add x y -> evalExpr x + evalExpr y
  Mul x y -> evalExpr x * evalExpr y
  Par x -> evalExpr x

module Two
  ( day2A
  , day2B
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

day2A :: BS.ByteString -> BS.ByteString
day2A = maybe "invalid input"
              (BS.pack . show . length . filter isValid)
      . traverse parse . BS.lines

parse :: BS.ByteString -> Maybe (Int, Int, Char, BS.ByteString)
parse bs = do
  (a, rest) <- BS.readInt bs
  (b, rest') <- BS.readInt (BS.tail rest)
  (c, rest'') <- BS.uncons (BS.tail rest')
  let pw = BS.drop 2 rest''
  pure (a, b, c, pw)

isValid :: (Int, Int, Char, BS.ByteString) -> Bool
isValid (a, b, c, pw) =
  let n = length $ BS.findIndices (== c) pw
   in n >= a && n <= b

day2B :: BS.ByteString -> BS.ByteString
day2B = maybe "invalid input"
              (BS.pack . show . length . filter isValidB)
      . traverse parse . BS.lines

isValidB :: (Int, Int, Char, BS.ByteString) -> Bool
isValidB (a, b, c, pw) = hasA `xor` hasB
  where hasA = BS.index pw (a - 1) == c
        hasB = BS.index pw (b - 1) == c

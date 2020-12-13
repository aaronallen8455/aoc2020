module Thirteen
  ( day13A
  , day13B
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.Maybe

day13A :: BS.ByteString -> BS.ByteString
day13A = maybe "invalid input" (BS.pack . show . solveA)
       . parseInputA . BS.lines

parseInputA :: [BS.ByteString] -> Maybe (Int, [Int])
parseInputA [a, b] = do
  (d, _) <- BS.readInt a
  bs <- traverse BS.readInt . filter (/= "x") $ BS.split ',' b
  pure (d, fst <$> bs)

solveA :: (Int, [Int]) -> Int
solveA (target, bs) = uncurry (*) . minimum $ do
  b <- bs
  let (d, m) = divMod target b
  if m == 0 then [((d * b) - target, b)]
            else [((d * b + b) - target, b)]

day13B :: BS.ByteString -> BS.ByteString
day13B = BS.pack . show
       . solveB . parseInputB . last . BS.lines

parseInputB :: BS.ByteString -> [(Integer, Integer)]
parseInputB b
  = mapMaybe sequence . zip [0..]
  $ fmap (fromIntegral . fst) . BS.readInt
      <$> BS.split ',' b

solveB :: [(Integer, Integer)] -> Integer
solveB xs = result where
  (initOff, initLcm) = last xs
  go (o, x) (off, l, c) =
    let offDiff = off - o
        newL = lcm l x
        Just newC = find ((== offDiff) . flip mod x) [c, c + l ..]
     in (o, newL, newC - offDiff)

  (_, _, result) = foldr go (initOff, initLcm, initLcm) xs


{-# LANGUAGE OverloadedStrings #-}
module Six
  ( day6A
  , day6B
  ) where

import           Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

day6A :: BS.ByteString -> BS.ByteString
day6A = BS.pack . show . sum
      . map (S.size . foldMap (BS.foldr S.insert S.empty))
      . groupBy (\a b -> a /= "" && b /= "")
      . BS.lines

day6B :: BS.ByteString -> BS.ByteString
day6B = BS.pack . show . sum
      . map (S.size . foldr1 S.intersection . map (BS.foldr S.insert S.empty))
      . groupBy (\a b -> a /= "" && b /= "")
      . BS.lines

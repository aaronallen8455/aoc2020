module SymList where

-- Both ends in constant time

type SymList a = ([a], [a])

toSL :: [a] -> SymList a
toSL = foldr (<|) ([],[])

fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

(<|) :: a -> SymList a -> SymList a
x <| (xs, ys)
  | null ys = ([x], xs)
  | otherwise = (x : xs, ys)

(|>) :: SymList a -> a -> SymList a
(xs, ys) |> x
  | null xs = (ys, [x])
  | otherwise = (xs, x : ys)

tailSL :: SymList a -> SymList a
tailSL ([x], ys) = (reverse vs, us) where
  (us, vs) = splitAt (length ys `div` 2) ys
tailSL (xs, ys)
  | null xs = ([], [])
  | otherwise = (tail xs, ys)

initSL :: SymList a -> SymList a
initSL (_, []) = ([], [])
initSL (xs, [y]) = (us, reverse vs) where
  (us, vs) = splitAt (length xs `div` 2) xs
initSL (xs, ys)
  | null ys = ([], [])
  | otherwise = (xs, tail ys)

unconsSL :: SymList a -> Maybe (a, SymList a)
unconsSL ([],[]) = Nothing
unconsSL l@(x:xs, ys) = Just (x, tailSL l)

lengthSL :: SymList a -> Int
lengthSL (xs, ys) = length xs + length ys

takeSL :: Int -> SymList a -> SymList a
takeSL 0 xs = ([], [])
takeSL n xs =
  case unconsSL xs of
    Nothing -> ([],[])
    Just (x,xs) -> x <| takeSL (n-1) xs

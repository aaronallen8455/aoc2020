{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module TwentyOne
  ( day21A
  , day21B
  ) where

import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector as V

type Ingredient = BS.ByteString
type Allergen = BS.ByteString
type Food = (S.Set Ingredient, [Allergen])
type FoodId = Int
type Foods = V.Vector Food

newtype MapSet k a = MapSet { unMapSet :: M.Map k (S.Set a) }
  deriving Show

instance (Ord k, Ord a) => Semigroup (MapSet k a) where
  MapSet a <> MapSet b = MapSet $ M.unionWith (<>) a b

instance (Ord k, Ord a) => Monoid (MapSet k a) where
  mempty = MapSet mempty

type AllergenMap = MapSet Allergen FoodId
type IngMap = MapSet Ingredient Allergen
type IngFood = MapSet Ingredient FoodId

data St =
  St
    { _foods :: Foods
    , _AlMap :: AllergenMap
    , _ingMap :: IngMap
    }

day21A :: BS.ByteString -> BS.ByteString
day21A = handleInput (BS.pack . show . solveA)

day21B :: BS.ByteString -> BS.ByteString
day21B = handleInput solveB

handleInput :: (Foods -> BS.ByteString) -> BS.ByteString -> BS.ByteString
handleInput f = f . V.fromList . map parseFood . BS.lines

solveA :: Foods -> Int
solveA fs = sum . map S.size . mapMaybe (ingFood M.!?)
          . M.keys . M.filter S.null . unMapSet
          $ removeAllergens fs
 where
  MapSet ingFood = mkIngFood fs

solveB :: Foods -> BS.ByteString
solveB = format . until (all isLeft) go . fmap Right
       . M.filter (not . S.null) . unMapSet . removeAllergens
  where
    go m = M.foldrWithKey remove m filtered where
      filtered = M.mapMaybe findSingles m
      findSingles = \case
        Right s | S.size s == 1, [x] <- toList s -> Just x
        _ -> Nothing
      remove k x = fmap (fmap $ S.delete x)
                 . M.insert k (Left x)
    format = BS.intercalate "," . map snd . sort . map swap . M.toList

removeAllergens :: Foods -> IngMap
removeAllergens fs = MapSet $ execState (M.traverseWithKey goAllergen alMap) ingMap
 where
  MapSet alMap = mkAllergenMap fs
  MapSet ingMap = mkIngMap fs

  goAllergen :: Allergen -> S.Set FoodId -> State (M.Map Ingredient (S.Set Allergen)) ()
  goAllergen a fIds =
    let allIngs = foldMap getIngs fIds
        sharedIngs = foldr1 S.intersection $ getIngs <$> toList fIds
        noAl = allIngs S.\\ sharedIngs
     in for_ noAl $
          modify' . M.adjust (S.delete a)

  getIngs = fst . (fs V.!)

parseFood :: BS.ByteString -> Food
parseFood bs = (S.fromList $ BS.words ing, allergens)
  where
    (ing, a) = BS.span (/= '(') bs
    allergens = map (BS.dropWhile (== ' '))
              . BS.split ','
              . BS.init
              $ BS.drop 10 a

mkAllergenMap :: Foods -> AllergenMap
mkAllergenMap = fold . V.imap mkMap
  where
    mkMap i (ing, allr) =
      foldMap (MapSet . (`M.singleton` S.singleton i)) allr

mkIngMap :: Foods -> IngMap
mkIngMap = foldMap go
  where
    go (ing, aller) = foldMap (\i -> MapSet $ M.singleton i allerS) ing
      where allerS = S.fromList aller

mkIngFood :: Foods -> IngFood
mkIngFood = fold . V.imap go
  where
    go i (ing, _) =
      foldMap (\x -> MapSet . M.singleton x $ S.singleton i) ing

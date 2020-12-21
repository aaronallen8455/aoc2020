{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Twenty
  ( day20A
  , day20B
  ) where

import           Control.Monad
import           Data.Array
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.IntMap.Strict as IM
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

type Edge = BS.ByteString
type EdgeMap = M.Map Edge (S.Set PieceId)
type PieceId = Int
type PieceMap = IM.IntMap Piece
type Coord = (Int, Int)
type Grid = Array Coord Piece
type Image = V.Vector BS.ByteString

data Piece =
  Piece
    { edges :: Edges
    , transforms :: [Transform]
    , pid :: PieceId
    , pic :: [BS.ByteString]
    } deriving Show

data Edges =
  Edges
    { top :: Edge
    , bottom :: Edge
    , left :: Edge
    , right :: Edge
    } deriving Show

data Transform
  = Rot Rotation
  | Flip
  deriving Show

applyTransform :: [Transform] -> Edges -> Edges
applyTransform ts e = foldr go e ts where
  go Flip Edges{..} =  Edges{left = right, right = left, ..}
  go (Rot r) Edges{..} =
    case r of
      R90 ->
        Edges
          { top = left
          , bottom = right
          , left = bottom
          , right = top
          }
      R180 ->
        Edges
          { top = bottom
          , bottom = top
          , left = right
          , right = left
          }
      R270 ->
        Edges
          { top = right
          , bottom = left
          , left = top
          , right = bottom
          }

applyFlip :: Bool -> Edges -> Edges
applyFlip f Edges{..} =
  if f
     then Edges{..}
     else Edges{left = right, right = left, ..}

getEdges :: Piece -> Edges
getEdges p = applyTransform (transforms p) $ edges p

edgesList :: Edges -> [Edge]
edgesList Edges{..} = [top, right, bottom, left]

orient :: Piece -> Edge -> Edge -> Piece
orient p le te = orientFlp $ orientRot p where
  orientRot p = p { transforms = maybe id (:) rot $ transforms p } where
    es = edgesList $ getEdges p
    (n, _) = span (/= te) es
    rot = case length n of
            0 -> Nothing
            1 -> Just $ Rot R270
            2 -> Just $ Rot R180
            _ -> Just $ Rot R90
  orientFlp p =
    let flp =
          if (/= le) . left $ getEdges p
             then Just Flip
             else Nothing
     in p { transforms = maybe id (:) flp $ transforms p }

data Rotation = R90 | R180 | R270 deriving (Show, Enum)

day20A :: BS.ByteString -> BS.ByteString
day20A = maybe "invalid input" (BS.pack . show . solveA)
       . traverse parsePiece
       . filter (/= [""])
       . groupBy (\x y -> not $ BS.null x || BS.null y)
       . BS.lines

day20B :: BS.ByteString -> BS.ByteString
day20B = maybe "invalid input" (BS.pack . show . roughness . mkPic . mkMaps)
       . traverse parsePiece
       . filter (/= [""])
       . groupBy (\x y -> not $ BS.null x || BS.null y)
       . BS.lines
  where
    mkPic (pm, em) = V.fromList . assemblePic $ buildGrid pm em
    countMonsters p = S.size $ foldMap (findMonsters p) seaMonsters
    countHashes = sum . fmap (BS.length . BS.filter (=='#'))
    roughness p = countHashes p - countMonsters p

solveA :: [Piece] -> Int
solveA = product . uncurry findCorners . mkMaps

mkMaps :: [Piece] -> (PieceMap, EdgeMap)
mkMaps ps = (pieceMap, edgeMap) where
  edgeMap = M.fromListWith (<>) $ do
    p <- ps
    e <- edgesList $ edges p
    pure (e, S.singleton $ pid p)
  pieceMap = IM.fromList $ do
    p <- ps
    pure (pid p, p)

findCorners :: PieceMap -> EdgeMap -> [PieceId]
findCorners pm em = concatMap S.toList $ M.keys corners where
  border = M.elems $ findBorder em
  corners = M.filter (== 2) . M.fromListWith (+) . zip border $ repeat 1

findBorder :: EdgeMap -> EdgeMap
findBorder = M.filter ((==1) . S.size)

normalize :: Edge -> Edge
normalize = join $ min . BS.reverse

buildGrid :: PieceMap -> EdgeMap -> Grid
buildGrid pm em = grid where
  c:orners = findCorners pm em

  buildBorder :: PieceId -> Edge -> [Piece]
  buildBorder p e =
    case S.toList . S.delete p <$> em M.!? e of
      Just [i]
        | Just np <- pm IM.!? i
        , let (_, _:x:y:z:_) = break (== e) . cycle . edgesList $ getEdges np -- works?
        , [t] <- filter notConnected [x,z]
        , let np' = orient np e t
        -> if i `elem` orners
              then [np']
              else np' : buildBorder (pid np') y
      _ -> error "non existing cell"

  cp = pm IM.! c
  ([l, t], [r, b]) = partition notConnected . edgesList $ getEdges cp -- works?
  ocp = orient cp l t
  topBorder = buildBorder c (right $ getEdges ocp)
  leftBorder = map (\p -> p { transforms = Flip : Rot R90 : transforms p })
             $! buildBorder c (bottom $ getEdges ocp)
  notConnected = (== 1) . S.size . (em M.!)
  s = pred . floor . sqrt . fromIntegral $ IM.size pm

  grid = array ((0, 0), (s, s)) $ do
    c <- [0..s]
    r <- [0..s]
    let coord = (r, c)
    pure
      if | coord == (0, 0) -> (coord, ocp)
         | r == 0 -> (coord, topBorder !! (c - 1))
         | c == 0 -> (coord, leftBorder !! (r - 1))
         | otherwise ->
             let l = right . getEdges $ grid ! (r, c - 1)
                 t = bottom . getEdges $ grid ! (r - 1, c)
                 [p] = S.toList $ (em M.! l) `S.intersection` (em M.! t)
              in (coord, orient (pm IM.! p) l t)

parsePiece :: [BS.ByteString] -> Maybe Piece
parsePiece (ib:s) = do
  (i, _) <- BS.readInt $ BS.drop 5 ib
  let ts = BS.transpose s
  pure Piece
    { edges = Edges
      { top = normalize $ head s
      , bottom = normalize $ last s
      , left = normalize $ head ts
      , right = normalize $ last ts
      }
    , transforms = []
    , pid = i
    , pic = s
    }

transformPic :: [Transform] -> [BS.ByteString] -> [BS.ByteString]
transformPic ts bs = foldr go bs ts where
  go Flip = map BS.reverse
  go (Rot R90) = map BS.reverse . BS.transpose
  go (Rot R180) = map BS.reverse . reverse
  go (Rot R270) = reverse . BS.transpose

assemblePic :: Grid -> [BS.ByteString]
assemblePic g = p where
  p = stitch $ (\p -> transformPic (transforms p) $ pic p) <$> g
  stitch :: Array Coord [BS.ByteString] -> [BS.ByteString]
  stitch = concatMap (foldr1 (zipWith (<>)) . map (trimBorder . snd))
         . groupBy (\a b -> fst (fst a) == fst (fst b))
         . assocs
  trimBorder = map (BS.init . BS.tail) . init . tail

seaMonsters :: [Image]
seaMonsters = do
    t <- [ [Rot R90]
         , [Rot R180]
         , [Rot R270]
         , []
         ]
    f <- [ [Flip]
         , []
         ]
    let tr = t ++ f
    pure . V.fromList $ transformPic tr monster
  where
    monster =
      [ "                  # "
      , "#    ##    ##    ###"
      , " #  #  #  #  #  #   "
      ]

findMonsters :: Image -> Image -> S.Set Coord
findMonsters grid monster = foldMap checkOffset cands where
  rows = V.length grid - 1
  cols = BS.length (V.head grid) - 1
  mRows = V.length monster - 1
  mCols = BS.length (V.head monster) - 1
  cands = (,) <$> [0..rows - mRows - 1] <*> [0..cols - mCols - 1]
  monCoords = filter inMonster $ (,) <$> [0..mRows] <*> [0..mCols]
  inMonster c = imgIdx c monster == '#'
  checkOffset (r, c) = fromMaybe S.empty $ foldrM checkPt S.empty monCoords
    where
    checkPt (mr, mc) =
        if imgIdx gc grid == '#'
           then Just . S.insert gc
           else const Nothing
      where
        gc = (mr + r, mc + c)
  imgIdx (r, c) = flip BS.index c . (V.! r)

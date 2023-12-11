{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day10(day10) where

import Utils hiding (floodFill)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S

data Cell = TL | H | TR | V | BR | BL | Start | Empty | Empty2 deriving (Eq, Show)
data Grid a = Grid {ncols :: Int, nrows :: Int, mp :: Map Coord a} deriving (Show)
type DistGrid = Grid (Cell, Maybe Int)


instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid r c m) = Grid r c $ fmap f m


(!) :: Grid a -> Coord -> a
(Grid nc nr g) ! c@(x,y)
  | x>=nc = error $ "Error - x too big in Grid!: " ++ show x
  | y>=nr = error $ "Error - y too big in Grid!: " ++ show y
  | otherwise = g M.! c


emptyGrid :: Int -> Int -> Grid a
emptyGrid c r = Grid c r M.empty


unions :: [Grid a] -> Grid a
unions gs
  | length cs /= 1 = error "All grids mucst have the same number of columns"
  | length rs /= 1 = error "All grids mucst have the same number of rows"
  | otherwise = Grid (head $ head cs) (head $ head rs) $ M.unions $ mp <$> gs
  where
    cs = group $ sort $ ncols <$> gs
    rs = group $ sort $ nrows <$> gs


unionWith :: (a -> a -> a) -> Grid a -> Grid a -> Grid a
unionWith f (Grid c1 r1 m1) (Grid c2 r2 m2)
  | c1/=c2 = error "Grids must have the same number of columns in unionsWith"
  | r1/=r2 = error "Grids must have the same number of rows in unionsWith"
  | otherwise = Grid c1 r1 $ M.unionWith f m1 m2


insert :: Coord -> a -> Grid a -> Grid a
insert k c (Grid nc nr g) = Grid nc nr $ M.insert k c g


inBounds :: Grid a -> (Int, Int) -> Bool
inBounds (Grid c r _) (x,y) = x>=0 && y>=0 && x<c && y<r


parseCell :: Char -> Cell
parseCell c
  | c == '7' = TR
  | c == 'J' = BR
  | c == 'L' = BL
  | c == 'F' = TL
  | c == '-' = H
  | c == '|' = V
  | c == 'S' = Start
  | c == '.' = Empty
  | otherwise = error "Not a tile"

e2, h, v :: (Cell, Bool)
e2 = (Empty2, False)
h = (H, True)
v = (V, True)

-- between the cell and the one on the right...
betweenH :: (Cell, Bool) -> (Cell, Bool) -> (Cell, Bool)
betweenH (_, False) _ = e2
betweenH _ (_, False) = e2
betweenH (Start, _) c = betweenH (startCell, True) c
betweenH c (Start, _) = betweenH c (startCell, True)
betweenH (TR, _) _ = e2
betweenH (BR, _) _ = e2
betweenH (BL, _) (TR, _) = h
betweenH (BL, _) (BR, _) = h
betweenH (BL, _) (H, _) = h
betweenH (BL, _) _ = e2
betweenH (TL, _) (TR, _) = h
betweenH (TL, _) (BR, _) = h
betweenH (TL, _) (H, _) = h
betweenH (TL, _) _ = e2
betweenH (H, _) (TR, _) = h
betweenH (H, _) (BR, _) = h
betweenH (H, _) (H, _) = h
betweenH (H, _) _ = e2
betweenH (V, _) _ = e2
betweenH (Empty, _) _ = e2
betweenH (Empty2, _) _ = e2

-- between the cell and the one below
betweenV :: (Cell, Bool) -> (Cell, Bool) -> (Cell, Bool)
betweenV (_, False) _ = e2
betweenV _ (_, False) = e2
betweenV (Start, _) c = betweenV (startCell, True) c
betweenV c (Start, _) = betweenV c (startCell, True)
betweenV (BR, _) _ = e2
betweenV (BL, _) _ = e2
betweenV (TR, _) (BR, _) = v
betweenV (TR, _) (BL, _) = v
betweenV (TR, _) (V, _) = v
betweenV (TR, _) _ = e2
betweenV (TL, _) (BR, _) = v
betweenV (TL, _) (BL, _) = v
betweenV (TL, _) (V, _) = v
betweenV (TL, _) _ = e2
betweenV (H, _) _ = e2
betweenV (V, _) (BL, _) = v
betweenV (V, _) (BR, _) = v
betweenV (V, _) (V, _) = v
betweenV (V, _) _ = e2
betweenV (Empty, _) _ = e2
betweenV (Empty2, _) _ = e2


-- So that there is a route for the floodfill to flood through
-- I put exta rows and cols between each row/col.
-- The path is extended across the extra rows/cols but the other cells are 
-- Empty2 (so that we know not to count them as INSIDE
expandGrid :: Grid (Cell, Bool) -> Grid (Cell, Bool)
expandGrid grid@(Grid nc nr _) = unions (expandV $ emptyGrid (2*nc-1) (2*nr-1))
  where
    wideGrid = unions (expandH $ emptyGrid (2*nc-1) (2*nr-1))

    expandH gg = (\row -> foldl (addColumns row) gg [0..(nc-1)]) <$> [0..(nr-1)]
    addColumns :: Int -> Grid (Cell, Bool) -> Int -> Grid (Cell, Bool)
    addColumns r g c
      | c==(nc-1) = insert (2*c,r) (grid ! (c,r)) g
      | otherwise = insert (2*c+1,r) (betweenH (grid ! (c,r)) (grid ! (c+1,r)) ) $ insert (2*c,r) (grid ! (c,r)) g

    expandV ggg = (\col -> foldl (addRows col) ggg [0..(nr-1)]) <$> [0..(2*nc-2)]
    addRows :: Int -> Grid (Cell, Bool) -> Int -> Grid (Cell, Bool)
    addRows c g r
      | r == (nr-1) = insert (c,2*r) (wideGrid ! (c,r)) g
      | otherwise = insert (c,2*r+1) (betweenV (wideGrid ! (c,r)) (wideGrid ! (c,r+1)) ) $ insert (c,2*r) (wideGrid ! (c,r)) g


parse :: [String] -> Grid Cell
parse css = Grid (length $ head css) (length css) $ M.fromList $ concatMap (\(y, cs) -> (\(x,c) -> ((x,y), parseCell c)) <$> zip [0..] cs) $ zip [0..] css


-- Work out where we need to have come from
startMoves :: Grid Cell -> [Coord]
startMoves g = fst <$> ns
  where
    start = getStart g
    ns =  filter p ts
      where
        ts = (\d -> (d, g ! (start+d))) <$> filter (\d -> inBounds g (start+d)) [lt, rt, up, dn]
        p (dir, cell)
          | dir == lt && cell `elem` [TL,BL,H] = True
          | dir == rt && cell `elem` [TR,BR,H] = True
          | dir == dn && cell `elem` [BL,BR,V] = True
          | dir == up && cell `elem` [TL,TR,V] = True
          | otherwise = False


move :: DistGrid -> (Coord, Coord) -> Maybe Coord
move g (p, from)
  | t == H && from==rt = Just $ p + lt
  | t == H && from==lt = Just $ p + rt
  | t == V && from==up = Just $ p + dn
  | t == V && from==dn = Just $ p + up
  | t == TL && from==dn = Just $ p + rt
  | t == TL && from==rt = Just $ p + dn
  | t == TR && from==dn = Just $ p + lt
  | t == TR && from==lt = Just $ p + dn
  | t == BL && from==up = Just $ p + rt
  | t == BL && from==rt = Just $ p + up
  | t == BR && from==up = Just $ p + lt
  | t == BR && from==lt = Just $ p + up
  | t == Start && from==up = Just $ p + lt
  | t == Start && from==lt = Just $ p + up
  | otherwise = Nothing
  where
    (t, _) = g ! p


getStart :: Grid Cell -> Coord
getStart (Grid _ _ g) = go $ M.keys g
  where
    go [] = error "There should be a start cell"
    go (k:ks)
      | g M.! k == Start = k
      | otherwise = go ks


minDist :: (a, Maybe Int) -> (a, Maybe Int) -> (a, Maybe Int)
minDist (c, mx) (_, my) = go mx my
  where
    go Nothing Nothing = (c, Nothing)
    go Nothing (Just x) = (c, Just x)
    go (Just x) Nothing = (c, Just x)
    go (Just x) (Just y) = (c, Just $ min x y)


makePath :: Coord -> Grid Cell -> DistGrid
makePath c grid = go (start, 0, c) $ (, Nothing) <$> grid
  where
    start = getStart grid
    go :: (Coord, Int, Coord) -> DistGrid -> DistGrid
    go (p, n, d) dg
      | isNothing nx = dg
      | isNothing (snd (dg ! fromJust nx)) = go (fromJust nx, n+1, p - fromJust nx) $ insert p (fst $ dg ! p, Just n) dg
      | otherwise = dg
      where
        nx = move dg (p,d)


-- Makes a flood map from a path map
floodFill :: Grid  Bool -> Grid Bool
floodFill pathGrid@(Grid c r _) = go (emptyGrid c r) $ S.fromList $ filter (\e -> not $ pathGrid ! e) $ allEdges pathGrid
  where
    go :: Grid Bool -> Set Coord -> Grid Bool
    go g@(Grid _ _ mp) pp 
      | S.null pp = g
      | otherwise = go (insert c True g) (pipeline <> ns)
      where
        (c, pipeline) = S.deleteFindMin pp
        ns = S.fromList $ filter (\d -> inBounds g d && not (pathGrid ! d) && d `M.notMember` mp) $ neighbours8 c


allEdges :: Grid a -> [(Int, Int)]
allEdges (Grid c r _) = concatMap (\y -> (,y) <$> [0..(c-1)]) [0, r-1] ++ concatMap (\y -> (,y) <$> [0,c-1]) [0..(r-1)]


startCell :: Cell
startCell = BR


day10 :: IO ()
day10 = do
  ls <- getLines 10
  let g = parse ls
      d1 = startMoves g !! 0
      d2 = startMoves g !! 1
      f1, f2, pathDist :: DistGrid
      f1 = makePath d1 g
      f2 = makePath d2 g
      pathDist@(Grid _ _ pathDistMap) = unionWith minDist f1 f2
      pathBool = second isJust <$> pathDist

      exPathBool@(Grid _ _ exPathBoolMap) = expandGrid pathBool
      ff'@(Grid _ _ mff') = floodFill $ snd <$> exPathBool
      ret' = M.filterWithKey (\k (c, mi) -> (c /= Empty2) && not mi && (k `M.notMember` mff' || not (ff' ! k))) exPathBoolMap

  putStrLn $ "Day10: part1: " ++ show (maximum $ mapMaybe (snd . snd) (M.toList pathDistMap))
  putStrLn $ "Day10: part2: " ++ show (M.size ret')

  return ()


-- --############################ GRAVEYARD OF CODE

showCell :: Cell -> Char
showCell c
  | c == TR = '7'
  | c == BR = 'J'
  | c == BL = 'L'
  | c == TL = 'F'
  | c == H = '-'
  | c == V = '|'
  | c == Start = 'S'
  | c == Empty = '.'
  | c == Empty2 = ','
  | otherwise = error "Error om showcell"


showGrid :: Grid Cell -> String
showGrid g@(Grid c r _) = unlines $ (\y -> (\x -> showCell $ g ! (x,y)) <$> [0..(c-1)]) <$> [0..(r-1)]


showM :: Show a => Maybe a -> Char
showM Nothing = '.'
showM x =  last $ show $ fromJust x

showDistGrid :: DistGrid -> String
showDistGrid g@(Grid c r _) = unlines $ (\y -> (\x -> showM . snd $ g ! (x,y)) <$> [0..(c-1)]) <$> [0..(r-1)]


showRegionGrid :: Grid (Maybe Int) -> String
showRegionGrid g@(Grid c r _) = unlines $ (\y -> (\x -> last $ maybe "." show $ g ! (x,y)) <$> [0..(c-1)]) <$> [0..(r-1)]


showBoolGrid :: Grid Bool -> String
showBoolGrid g@(Grid c r mp) = unlines $ (\y -> (\x -> (if (x,y) `M.notMember` mp then '.' else if g ! (x,y) then '#' else '.')) <$> [0..(c-1)]) <$> [0..(r-1)]


mapWithKey :: (Coord -> a -> b) -> Grid a -> Grid b
mapWithKey f (Grid c r mp) = Grid c r $ M.mapWithKey f mp




module Day13(day13) where

import Utils hiding (transpose)
import Data.Set (Set)
import Data.Set qualified as S


type Grid = (Set Coord, Coord)

transpose :: Grid -> Grid
transpose (g, c) = (S.map swap g, swap c)


parseGrid :: [String] -> Grid
parseGrid css = (S.fromList $ concatMap (catMaybes . (\(y, cs) -> (\(x, c) -> if c=='#' then Just (x,y) else Nothing) <$> zip [0..] cs)) (zip [0..] css), (length $ css!!0, length css))


parse :: [String] -> [Grid]
parse s = parseGrid <$> gs
  where
    gs = splitOn "" s


isReflector :: Grid -> Int -> Bool
isReflector (cs, (maxx, _)) n = l == r
  where
    l, r :: Set Coord
    l = S.map (first (n-)) $ S.filter (\(x,_) -> (x<=n) && x>(2*n-maxx+1)) cs
    r = S.map (first (-(n+1)+)) $ S.filter (\(x,_) -> (x>n) && (x<=2*n+1)) cs


isOneOff :: Grid -> Int -> Bool
isOneOff (cs, (maxx, _)) n = (ln /= rn) && ((ln>rn) && (dl==1)) || ((rn>ln) && (dr==1))
  where
    ln = S.size l
    rn = S.size r
    dl = S.size (l S.\\ r)
    dr = S.size (r S.\\ l)
    l, r :: Set Coord
    l = S.map (first (n-)) $ S.filter (\(x,_) -> (x<=n) && x>(2*n-maxx+1)) cs
    r = S.map (first (-(n+1)+)) $ S.filter (\(x,_) -> (x>n) && (x<=2*n+1)) cs


find :: (Grid -> Int -> Bool) -> Grid -> Int
find p g@(_, (mx, my))
  | isJust h = 1 + fromJust h
  | otherwise = (1 + fromJust v) * 100
  where
    h = go (mx-2) g
    v = go (my-2) $ transpose g
    go n gg
      | n < 0 = Nothing
      | p gg n = Just n
      | otherwise = go (n-1) gg


day13 :: IO ()
day13 = do
  ls <- getLines 13
  let g = parse ls

  putStrLn $ "Day13: part2: " ++ show (sum $ find isReflector <$> g)
  putStrLn $ "Day13: part2: " ++ show (sum $ find isOneOff <$> g)

  return ()


module Day23(day23) where

import Utils (Coord, transpose, rt, lt, up, dn, neighbours4, getLines) 
import Data.Array.Unboxed qualified as A
import Data.Set qualified as S
import Data.Map qualified as M


type Ground = Char
type Grid = A.UArray Coord Ground
type Steps = Int
type Graph = M.Map Coord [(Coord, Steps)]


parse :: [String] -> A.UArray Coord Ground
parse xs = A.amap id $ A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat $ transpose xs)


inBounds :: Grid -> Coord -> Bool
inBounds g (x,y) = x<=mx && x>=0 && y<=my && y>=0
  where
    (mx, my) = snd $ A.bounds g


parseSlope :: Grid -> Coord -> Maybe Coord
parseSlope g p = go (g A.! p)
  where
    go '#' = Nothing
    go '.' = Nothing
    go '>' = Just rt
    go '<' = Just lt
    go '^' = Just up
    go 'v' = Just dn
    go c = error $ "Error in parseSlope: " ++ show [c]


nextState1 :: Grid -> Coord -> [Coord]
nextState1 g pos = case parseSlope g pos of
                    Nothing -> filter (\n -> inBounds g n && g A.! n /= '#') (neighbours4 pos)
                    Just d -> [pos+d]


nextState2 :: Grid -> Coord -> [Coord]
nextState2 g pos = filter (\n -> inBounds g n && g A.! n /= '#') (neighbours4 pos)            


makeGraph :: Grid -> (Coord -> [Coord])-> Graph
makeGraph g next = M.fromList $ (\n -> (n, dfs n)) <$> allNodes
  where
    (mx, my) = snd $ A.bounds g
    start, end :: Coord
    start = (1,0)
    end = (mx-1, my)
    ixs = A.indices g
    -- nodes must not be '#' and must have more than 2 exits
    allNodes = start : filter (\ix -> length [n | g A.! ix /= '#', n <- next ix, g A.! n /= '#'] > 2) ixs ++ [end]

    dfs :: Coord -> [(Coord, Steps)]
    dfs pos = concatMap (\p -> go (p,1) $ S.singleton pos) nbrs
      where
        nbrs = nextState1 g pos
        go (p,s) seen
          | p == pos = []
          | p `elem` allNodes = [(p,s)]
          | otherwise = concatMap (\n -> go (n, s+1) newSeen) ns
          where
            ns = filter (`notElem` seen) $ next p
            newSeen = S.insert p seen


solve ::  Graph -> Int
solve gr = maximum $ dfs (1,0) (S.singleton (1,0)) 0
  where
    end = (139,140)
    dfs :: Coord -> S.Set Coord ->  Int -> [Int]
    dfs pos seen maxPath
      | pos == end = [maxPath]
      | otherwise = concatMap (\(c,s) -> dfs c (S.insert c seen) $ maxPath+s) ns
      where
        ns = filter ((`S.notMember` seen) . fst) $ gr M.! pos


day23 :: IO ()
day23 = do
  ss <- getLines 23
  let g = parse ss

  putStrLn $ "Day23: part1: " ++ show (solve $ makeGraph g $ nextState1 g)
  putStrLn $ "Day23: part1: " ++ show (solve $ makeGraph g $ nextState2 g) -- 6802

  return ()


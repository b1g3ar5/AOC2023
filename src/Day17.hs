{-# OPTIONS_GHC -Wno-orphans #-}

module Day17(day17) where

import Prelude hiding (null)
import Utils
import HeapIndexed
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Loss = Int
type Grid = (M.Map Coord Loss, Int, Int)
type State = (Coord, Coord, Int) -- position, direction, count


parse :: [String] -> Grid 
parse css = (M.fromList $ concatMap (\(y,cs) -> (\(x,c) -> ((x,y), read [c])) <$> zip [0..] cs) $ zip [0..] css, length $ css!!0, length css)


inBounds :: Grid -> Coord -> Bool
inBounds (_, mx, my) (x,y) = x<mx && x>=0 && y<my && y>=0

nextStates :: (Int, Int) -> Grid -> State -> [State]
nextStates (lo, hi) g (pos, direction, count)
  | count<lo = [(pos + direction, direction, count+1) | inBounds g (pos + direction)]
  | count==hi = (\d -> (pos + d, d, 1)) <$> filter (/= direction) ds
  | otherwise = (\d -> (pos + d, d, if d==direction then count+1 else 1)) <$> ds
    where
      ds = filter (\d -> inBounds g (pos + d) && (d /= (- direction))) neighbourCoords4


cost :: Grid -> State -> Loss
cost (mp,_,_) (p,_,_)= mp M.! p


finish :: Int -> Grid -> State -> Bool
finish hi (_,mx,my) ((x,y),_, c) = (x == mx-1) && (y == my-1) && c>=hi


dijkstra :: Grid -> Heap Loss State -> S.Set State -> (State -> [State]) -> (State -> Bool) -> Loss
dijkstra g pipeline visited next finished
  | null pipeline = error "No pipeline left"
  | finished state = savedMin
  | state `S.member` visited = dijkstra g remainingPipeline visited next finished
  | otherwise = dijkstra g newPipeline (S.insert state visited) next finished
  where
    ((savedMin, state), remainingPipeline) = fromJust $ extractMin pipeline
    newStates = next state
    newPipeline = remainingPipeline `union` fromList ((\n -> (savedMin + cost g n, n)) <$> newStates)



solve :: Grid -> (State -> [State]) -> (State -> Bool) -> Loss
--solve g = dijkstra g (singleton 0 ((0,0), dn, 0)) S.empty
solve g = dijkstra g (fromList [(0, ((0,0), rt, 0)), (0, ((0,0), dn, 0))]) S.empty


day17 :: IO ()
day17 = do
  xs <- getLines 17
  let g = parse xs


  putStrLn $ "Day17: part2: " ++ show (solve g (nextStates (0, 3) g) (finish 0 g))
  putStrLn $ "Day17: part2: " ++ show (solve g (nextStates (4, 10) g) (finish 4 g)) -- 1202 too high, 1200 too low

  return ()


test = [
    "2413432311323"
  , "3215453535623"
  , "3255245654254"
  , "3446585845452"
  , "4546657867536"
  , "1438598798454"
  , "4457876987766"
  , "3637877979653"
  , "4654967986887"
  , "4564679986453"
  , "1224686865563"
  , "2546548887735"
  , "4322674655533"]

test1 = ["111111111111"
  , "999999999991"
  , "999999999991"
  , "999999999991"
  , "999999999991"]

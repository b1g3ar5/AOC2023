{-# OPTIONS_GHC -Wno-orphans #-}

module Day17(day17) where

import Prelude hiding (null)
import Utils (transpose, Coord, getLines, neighbourCoords4, rt, dn)
import Data.PQueue.Min qualified as Q
import qualified Data.Set as S
import Data.Array.Unboxed qualified as A


type Loss = Int
type Grid = A.UArray Coord Loss
type State = (Coord, Coord, Int) -- position, direction, count


-- REMEBER to transpose the stings for (x,y) coordinate
-- Without the transpose its (row, col)
parse :: [String] -> Grid 
parse xs = A.amap (read . (:[])) $ A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat $ transpose xs)


inBounds :: Grid -> Coord -> Bool
inBounds g (x,y) = x<=mx && x>=0 && y<=my && y>=0
  where
    (mx, my) = snd $ A.bounds g


nextStates :: (Int, Int) -> Grid -> State -> [State]
nextStates (lo, hi) g (pos, direction, count)
  | count<lo = [(pos + direction, direction, count+1) | inBounds g (pos + direction)]
  | count==hi = (\d -> (pos + d, d, 1)) <$> filter (/= direction) ds
  | otherwise = (\d -> (pos + d, d, if d==direction then count+1 else 1)) <$> ds
    where
      ds = filter (\d -> inBounds g (pos + d) && (d /= (- direction))) neighbourCoords4


cost :: Grid -> State -> Loss
cost g (p,_,_)= g A.! p


finish :: Int -> Grid -> State -> Bool
finish hi g ((x,y),_, c) = (x == mx) && (y == my) && c>=hi
  where
    (mx, my) = snd $ A.bounds g


dijkstra :: Grid -> Q.MinQueue (Loss, State) -> S.Set State -> (State -> [State]) -> (State -> Bool) -> Loss
dijkstra g pipeline visited next finished
  | Q.null pipeline = error "No pipeline left"
  | finished state = savedMin
  | state `S.member` visited = dijkstra g remainingPipeline visited next finished
  | otherwise = dijkstra g newPipeline (S.insert state visited) next finished
  where
    ((savedMin, state), remainingPipeline) = Q.deleteFindMin pipeline
    newStates = next state
    newPipeline = remainingPipeline `Q.union` Q.fromList ((\n -> (savedMin + cost g n, n)) <$> newStates)


solve :: Grid -> (State -> [State]) -> (State -> Bool) -> Loss
solve g = dijkstra g (Q.fromList [(0, ((0,0), rt, 0)), (0, ((0,0), dn, 0))]) S.empty


day17 :: IO ()
day17 = do
  xs <- getLines 17
  let g = parse xs

  putStrLn $ "Day17: part2: " ++ show (solve g (nextStates (0, 3) g) (finish 0 g))
  putStrLn $ "Day17: part2: " ++ show (solve g (nextStates (4, 10) g) (finish 0 g))


  return ()

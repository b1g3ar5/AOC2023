module Day16(day16) where


import Utils (dn, getLines, lt, rt, up, Coord, transpose)
--import Data.Map (Map)
--import Data.Map qualified as M
import Data.Array.IArray (Array, amap, bounds, (!), listArray, elems)
import Data.Set (Set)
import Data.Set qualified as S
import Control.Parallel.Strategies


data Cell = H | V | B | F | E deriving (Eq, Show)
type Direction = Coord


parseCell :: Char -> Cell
parseCell '|' = V
parseCell '-' = H
parseCell '/' = F
parseCell '\\' = B
parseCell c = E 


parse :: [String] -> Array Coord Cell
parse xs = amap parseCell $ listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat $ transpose xs)


reflection :: (Direction, Cell) -> [Direction]
reflection (d, H)
  | d==lt || d==rt = [d]
  | otherwise = [lt, rt] 
reflection (d, V)
  | d==up  || d==dn = [d]
  | otherwise = [up, dn] 
reflection ((x,y), B) = [(y,x)]
reflection ((x,y), F) = [(-y,-x)]
reflection (d, E) = [d]


run :: Array Coord Cell -> (Coord, Direction) -> Set (Coord, Direction)
run g (p,d) = go S.empty (p, d)
  where
    go :: Set (Coord, Direction) -> (Coord, Direction) -> Set (Coord, Direction)
    go acc (pos, dir)
      | (pos, dir) `S.member` acc = acc
      | not (inBounds pos) = acc
      | otherwise = foldl (go . S.insert (pos, dir)) acc $ zip newCoords newDirections
      where
        cell = g ! pos
        newDirections = reflection (dir, cell)
        newCoords = (pos +) <$>  newDirections


size :: Int
size = 109

setOff :: [(Coord, Direction)]
setOff = ((\y -> ((0,y), rt)) <$> [0..size])
         ++ ((\y -> ((size,y), lt)) <$> [0..size])
         ++ ((\x -> ((x ,0), dn)) <$> [0..size])
         ++ ((\x -> ((x,size), up)) <$> [0..size])


inBounds :: Coord -> Bool
inBounds (x,y) = x<=size && y<=size && x>=0 && y>=0


day16 :: IO ()
day16 = do
  ss <- getLines 16
  let g = parse ss
      xs' = parMap rpar (run g) setOff

  putStrLn $ "Day16: part1: " ++ show (S.size $ S.map fst $ run g ((0,0), rt))
  putStrLn $ "Day16: part2: " ++ show (maximum $ S.size . S.map fst <$> xs') 

  return ()

module Day16(day16) where


import Utils (dn, getLines, lt, rt, up, Coord, timeIt)
import Data.Map (Map)
import Data.Map qualified as M
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


parse :: [String] -> (Map Coord Cell, Int, Int)
parse css = (M.fromList $ concatMap (\(y, cs) -> (\(x,c) -> ((x,y), parseCell c)) <$> zip [0..] cs ) $ zip [0..] css, length $ css!!0, length css)


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


run :: (Map Coord Cell, Int, Int) -> (Coord, Direction) -> Set (Coord, Direction)
run (g, mx, my) (p,d) = go S.empty (p, d)
  where
    go :: Set (Coord, Direction) -> (Coord, Direction) -> Set (Coord, Direction)
    go acc (pos, dir)
      | (pos, dir) `S.member` acc = acc
      | not (inBounds pos mx my) = acc
      | otherwise = foldl (go . S.insert (pos, dir)) acc $ zip newCoords newDirections
      where
        cell = g M.! pos
        newDirections = reflection (dir, cell)
        newCoords = (pos +) <$>  newDirections


size :: Int
size = 109

setOff :: [(Coord, Direction)]
setOff = ((\y -> ((0,y), rt)) <$> [0..size])
         ++ ((\y -> ((size,y), lt)) <$> [0..size])
         ++ ((\x -> ((x ,0), dn)) <$> [0..size])
         ++ ((\x -> ((x,size), up)) <$> [0..size])


inBounds :: Coord -> Int -> Int -> Bool
inBounds (x,y) mx my = x<mx && y<my && x>=0 && y>=0


day16 :: IO ()
day16 = do
  ss <- getLines 16
  let g = parse ss
      --xs1 = fmap (run g) setOff
      xs2 = parMap rpar (run g) setOff

  putStrLn $ "Day16: part1: " ++ show (S.size $ S.map fst $ run g ((0,0), rt))
  --timeIt $ putStrLn $ "Day16: part2: " ++ show (maximum $ S.size . S.map fst <$> xs1) 
  timeIt $ putStrLn $ "Day16: part2: " ++ show (maximum $ S.size . S.map fst <$> xs2) 

  return ()

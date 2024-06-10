module Day11(day11) where

import Data.List ((\\))
import Data.Maybe (catMaybes)


type Coord = (Int, Int)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".in"
  return $ f s


getLines :: Int -> IO [String]
getLines = getF lines


parse :: [String] -> [Coord]
parse css = concatMap (catMaybes . (\(y, cs) -> (\(x, c) -> if c=='#' then Just (x,y) else Nothing) <$> zip [0..] cs)) (zip [0..] css)


nSize :: Int
nSize = 140


bigManhattan :: Int -> [Coord] -> [(Coord, Coord)] -> Int
bigManhattan k galaxies = foldl (\acc p -> acc + go p) 0
  where
    baseC, baseR :: [Int]
    baseC = [0..(nSize-1)] \\ (fst <$> galaxies)
    baseR = [0..(nSize-1)] \\  (snd <$> galaxies)
    
    go ((c1, r1), (c2, r2)) = manhattan (c1+newc1, r1+newr1) (c2+newc2, r2+newr2)
      where
        newc1, newc2, newr1, newr2 :: Int
        newc1 = k * length (filter (c1>) baseC)
        newc2 = k * length (filter (c2>) baseC)
        newr1 = k * length (filter (r1>) baseR)
        newr2 = k * length (filter (r2>) baseR)



day11 :: IO ()
day11 = do
  ls <- getLines 11
  let galaxies = parse ls
      pairs = [(x,y) | !x <- galaxies, !y <- galaxies, x<y ]

  putStrLn $ "Day11: part1: " ++ show (bigManhattan 1 galaxies pairs)
  putStrLn $ "Day11: part2: " ++ show (bigManhattan 999999 galaxies pairs)
  return ()




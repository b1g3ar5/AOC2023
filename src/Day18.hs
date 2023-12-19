module Day18(day18) where

import Utils (dn, getLines, lt, manhattan, rt, up, Coord)


parseDirection :: Char -> Coord
parseDirection 'U' = up
parseDirection 'D' = dn
parseDirection 'L' = lt
parseDirection 'R' = rt
parseDirection '0' = rt
parseDirection '1' = dn
parseDirection '2' = lt
parseDirection '3' = up
parseDirection c = error [c]


parse1 :: String -> (Coord, Int)
parse1 s = (parseDirection $ head $ ws!!0, read $ ws!!1) --, tail $ init $ ws!!2)
  where
    ws = words s


readHex :: Char -> Int
readHex 'a' = 10
readHex 'b' = 11
readHex 'c' = 12
readHex 'd' = 13
readHex 'e' = 14
readHex 'f' = 15
readHex c = read [c]


parse16 :: String -> Int
parse16 = go . reverse
  where
    go [] = 0
    go (d:ds) = readHex d + 16 * go ds


parse2 :: String -> (Coord, Int)
parse2 s = (parseDirection $ last w3, parse16 $ init w3) --, tail $ init $ ws!!2)
  where
    ws = words s
    w3 = take 6 $ drop 2 $ ws!!2


path :: [(Coord, Int)] -> [Coord]
path = go [(0,0)] (0,0)
  where
    go acc _ [] = acc
    go acc p ((d,n):others) = go (acc ++ f) (last f) others
      where
        f = [p + d*(n,n)]


-- Shoelace formula:  A = 0.5*sum_1^n (x_i*(y_i+1 - y_i-1))
-- Calculates the area of a polygon from the cartesian coordinates
shoelace :: [Coord] -> Int
shoelace cs = abs $ sum (go <$> [0..(n-1)]) `div` 2
  where
    n = length cs
    go i = fst (cs!!i) * (snd (cs!!((i+1) `mod` n)) - snd (cs!!((i-1) `mod` n)) )


pathLength :: [Coord] -> Int
pathLength cs = foldl (\n (f,t)-> n + manhattan t f) 0 $ zip cs (tail cs ++ [head cs])


-- Picks Theorum: A = i + b/2 - 1 where A = area, i = interior points, b = boundary points
-- Relates the area of a polygon to the number of interior points and boundary points
-- We know the area from the Shoelace formula and the boundary points to we can calculate the interior points
-- The area we want is just the number of interior points plus the boundary points
pick :: [Coord] -> Int
pick p = i + b
  where
    a = shoelace p
    b = pathLength p
    i = a - b `div` 2 + 1


day18 :: IO ()
day18 = do
  ls <- getLines 18

  putStrLn $ "Day18: part2: " ++ show (pick $ init $ path $ parse1 <$> ls)
  putStrLn $ "Day18: part2: " ++ show (pick $ init $ path $ parse2 <$> ls)

  return ()


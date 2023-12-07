module Day6(day6) where


--scoreSlow :: (Int, Int) -> Int
--scoreSlow (t, d) = length $ filter (> d) $ (\s -> s*(t-s)) <$> [0..t]

score :: (Int, Int) -> Int
score (t,d) = t - 2*x + 1
  where
    x = binarySearch  (\y -> y * (t-y) > d) 1 (t `div` 2)

day6 :: IO ()
day6 = do
  let g1 = [(41,249), (77,1362), (70,1127), (96,1011)]
      g2= (41777096, 249136211271011)

  putStrLn $ "Day6: part1: " ++ show (product $ score <$> g1)
  putStrLn $ "Day6: part2: " ++ show (score g2)

  return ()


binarySearch :: Integral t => (t -> Bool) -> t -> t -> t
binarySearch p = go
  where
    go lo hi
      | lo + 1 == hi = hi
      | p mid = go lo mid
      | otherwise = go mid hi
      where
        mid = (lo + hi) `div` 2
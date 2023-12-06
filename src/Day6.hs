module Day6(day6) where


score :: (Int, Int) -> Int
score (t, d) = length $ filter (> d) $ (\s -> s*(t-s)) <$> [0..t]


day6 :: IO ()
day6 = do
  let gs = [(41,249), (77,1362), (70,1127), (96,1011)]
      g = (41777096, 249136211271011)

  putStrLn $ "Day6: part1: " ++ show (product $ score <$> gs)
  putStrLn $ "Day6: part2: " ++ show (score g)

  return ()

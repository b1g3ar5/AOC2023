module Day17(day17) where

import Utils


parse :: String -> Int
parse = read


day17 :: IO ()
day17 = do
  ss <- getLines 17
  let g = parse <$> ss

  putStrLn $ "Day17: part1: " ++ show g
  putStrLn $ "Day17: part2: " ++ show ""

  return ()

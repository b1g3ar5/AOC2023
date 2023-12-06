module Day12(day12) where

import Utils


parse :: String -> Int
parse = read


day12 :: IO ()
day12 = do
  ss <- getLines 12
  let g = parse <$> ss

  putStrLn $ "Day12: part1: " ++ show g
  putStrLn $ "Day12: part2: " ++ show ""

  return ()

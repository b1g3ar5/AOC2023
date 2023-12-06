module Day10(day10) where

import Utils


parse :: String -> Int
parse = read


day10 :: IO ()
day10 = do
  ss <- getLines 10
  let g = parse <$> ss

  putStrLn $ "Day10: part1: " ++ show g
  putStrLn $ "Day10: part2: " ++ show ""

  return ()

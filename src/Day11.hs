module Day11(day11) where

import Utils


parse :: String -> Int
parse = read


day11 :: IO ()
day11 = do
  ss <- getLines 11
  let g = parse <$> ss

  putStrLn $ "Day11: part1: " ++ show g
  putStrLn $ "Day11: part2: " ++ show ""

  return ()

module Day23(day23) where

import Utils


parse :: String -> Int
parse = read


day23 :: IO ()
day23 = do
  ss <- getLines 23
  let g = parse <$> ss

  putStrLn $ "Day23: part1: " ++ show g
  putStrLn $ "Day23: part2: " ++ show ""

  return ()

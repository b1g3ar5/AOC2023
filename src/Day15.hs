module Day15(day15) where

import Utils


parse :: String -> Int
parse = read


day15 :: IO ()
day15 = do
  ss <- getLines 15
  let g = parse <$> ss

  putStrLn $ "Day15: part1: " ++ show g
  putStrLn $ "Day15: part2: " ++ show ""

  return ()

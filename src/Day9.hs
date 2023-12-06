module Day9(day9) where

import Utils


parse :: String -> Int
parse = read


day9 :: IO ()
day9 = do
  ss <- getLines 9
  let g = parse <$> ss

  putStrLn $ "Day9: part1: " ++ show g
  putStrLn $ "Day9: part2: " ++ show ""

  return ()

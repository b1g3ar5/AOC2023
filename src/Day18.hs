module Day18(day18) where

import Utils


parse :: String -> Int
parse = read


day18 :: IO ()
day18 = do
  ss <- getLines 18
  let g = parse <$> ss

  putStrLn $ "Day18: part1: " ++ show g
  putStrLn $ "Day18: part2: " ++ show ""

  return ()

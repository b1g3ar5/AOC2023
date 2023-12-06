module Day20(day20) where

import Utils


parse :: String -> Int
parse = read


day20 :: IO ()
day20 = do
  ss <- getLines 20
  let g = parse <$> ss

  putStrLn $ "Day20: part1: " ++ show g
  putStrLn $ "Day20: part2: " ++ show ""

  return ()

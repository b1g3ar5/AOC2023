module Day19(day19) where

import Utils


parse :: String -> Int
parse = read


day19 :: IO ()
day19 = do
  ss <- getLines 19
  let g = parse <$> ss

  putStrLn $ "Day19: part1: " ++ show g
  putStrLn $ "Day19: part2: " ++ show ""

  return ()

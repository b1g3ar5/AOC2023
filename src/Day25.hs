module Day25(day25) where

import Utils


parse :: String -> Int
parse = read


day25 :: IO ()
day25 = do
  ss <- getLines 25
  let g = parse <$> ss

  putStrLn $ "Day25: part1: " ++ show g
  putStrLn $ "Day25: part2: " ++ show ""

  return ()

module Day21(day21) where

import Utils


parse :: String -> Int
parse = read


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let g = parse <$> ss

  putStrLn $ "Day21: part1: " ++ show g
  putStrLn $ "Day21: part2: " ++ show ""

  return ()

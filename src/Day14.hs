module Day14(day14) where

import Utils


parse :: String -> Int
parse = read


day14 :: IO ()
day14 = do
  ss <- getLines 14
  let g = parse <$> ss

  putStrLn $ "Day14: part1: " ++ show g
  putStrLn $ "Day14: part2: " ++ show ""

  return ()

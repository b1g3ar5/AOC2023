module Day24(day24) where

import Utils


parse :: String -> Int
parse = read


day24 :: IO ()
day24 = do
  ss <- getLines 24
  let g = parse <$> ss

  putStrLn $ "Day24: part1: " ++ show g
  putStrLn $ "Day24: part2: " ++ show ""

  return ()

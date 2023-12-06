module Day13(day13) where

import Utils


parse :: String -> Int
parse = read


day13 :: IO ()
day13 = do
  ss <- getLines 13
  let g = parse <$> ss

  putStrLn $ "Day13: part1: " ++ show g
  putStrLn $ "Day13: part2: " ++ show ""

  return ()

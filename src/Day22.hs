module Day22(day22) where

import Utils


parse :: String -> Int
parse = read


day22 :: IO ()
day22 = do
  ss <- getLines 22
  let g = parse <$> ss

  putStrLn $ "Day22: part1: " ++ show g
  putStrLn $ "Day22: part2: " ++ show ""

  return ()

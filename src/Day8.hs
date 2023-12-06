module Day8(day8) where

import Utils


parse :: String -> Int
parse = read


day8 :: IO ()
day8 = do
  ss <- getLines 8
  let g = parse <$> ss

  putStrLn $ "Day8: part1: " ++ show g
  putStrLn $ "Day8: part2: " ++ show ""

  return ()

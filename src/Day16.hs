module Day16(day16) where

import Utils


parse :: String -> Int
parse = read


day16 :: IO ()
day16 = do
  ss <- getLines 16
  let g = parse <$> ss

  putStrLn $ "Day16: part1: " ++ show g
  putStrLn $ "Day16: part2: " ++ show ""

  return ()

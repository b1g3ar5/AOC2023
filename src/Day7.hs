module Day7(day7) where

import Utils


parse :: String -> Int
parse = read


day7 :: IO ()
day7 = do
  ls <- getLines 7
  let g = parse <$> ls

  putStrLn $ "Day7: part1: " ++ show g
  putStrLn $ "Day7: part2: " ++ show ""

  return ()

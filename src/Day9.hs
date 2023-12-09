module Day9(day9) where

import Utils (getLines)


extrap :: [Int] -> Int
extrap ys = if head ys == 0 then 0 else last ys + extrap (zipWith (-) (tail ys) ys)


day9 :: IO ()
day9 = do
  ls <- getLines 9
  let g :: [[Int]]
      g = (read <$>) . words <$> ls

  putStrLn $ "Day9: part2: " ++ show (sum $ extrap <$> g)
  putStrLn $ "Day9: part2: " ++ show (sum $ extrap . reverse <$> g)

  return ()


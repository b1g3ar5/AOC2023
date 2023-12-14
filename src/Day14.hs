module Day14(day14) where

import Utils (intercalate, sort, transpose, getLines)
import Data.Map qualified as M
import Data.List.Split qualified as S


tiltRt, tiltLt, tiltUp, tiltDn :: [String] -> [String]
tiltRt ls  = go <$> ls
  where
    go :: String -> String
    go s = intercalate "#" $ sort <$> S.splitOn "#" s
tiltDn = transpose . tiltRt . transpose
tiltLt = (reverse <$>). tiltRt . (reverse <$>)
tiltUp = reverse . tiltDn . reverse


cycle1 :: [String] -> [String]
cycle1 = tiltRt . tiltDn . tiltLt . tiltUp


score :: [String] -> Int
score ls = sum $ go <$> zip [0..] ls
  where
    my = length ls
    go :: (Int, String) -> Int
    go(n, cs) = (my - n) * length (filter (=='O') cs)


run :: Int -> [String] -> Int
run  = go M.empty
  where
    go seen n ls
      | n==0 = s
      | ls `M.member` seen && n<cyc = go newSeen (n-1) $ cycle1 ls
      | ls `M.member` seen = go newSeen (n-reps*cyc-1) $ cycle1 ls
      | otherwise = go newSeen (n-1) $ cycle1 ls
      where
        cyc = seen M.! ls - n
        reps = n `div` cyc
        s = score ls
        newSeen = M.insert ls n seen

day14 :: IO ()
day14 = do
  ls <- getLines 14

  putStrLn $ "Day14: part1: " ++ show (score $ tiltUp ls)
  putStrLn $ "Day14: part2: " ++ show (run 1000000000 ls)

  return ()




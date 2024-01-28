module Day21(day21) where

import Utils
import Data.Set qualified as S
import Numeric.LinearAlgebra


parse :: Int -> [String] -> (S.Set Coord, Int, Int)
parse sz css = (S.fromList $ (5,5) : concatMap (catMaybes . (\(y,cs) -> (\(x,c) -> if c `elem` "S." then Just (x,y) else Nothing) <$> zip [0..] cs)) (zip [0..] css), (-sz) + length (css!!0), -sz + length css)


positions :: Int -> Coord -> S.Set Coord -> S.Set Coord
positions steps start mp = go 0 (S.singleton start)
  where
    go n cs
      | n == steps = cs
      | otherwise = go (n+1) ns
      where
        ns = S.filter (\(x,y) -> (x `mod` 131, y `mod` 131) `S.member` mp) $ S.unions $ S.map (S.fromList . neighbours4) cs


fitQuadratic :: [Double] -> [Double] -> (Int, Int, Int)
fitQuadratic xs ys = (round $ cs!!0, round $ cs!!1, round $ cs!!2)
  where
    cs = toList $ m <\> fromList ys
    m = (3><3) $ concatMap (\x -> [x*x, x, 1]) xs


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let g@(cs, _, _) = parse sz ss
      steps1 = 64
      steps2 = 26501365 --- 202300*131+65
      k, sz :: Int
      (k, sz) = steps2 `divMod` 131
      
      steps = (\x -> sz + x*131) <$> [0,1,2]
      ys = (\s -> S.size $ positions s (sz,sz) cs) <$> steps
      (a,b,c) = fitQuadratic [0,1,2] (fromIntegral <$> ys)

  putStrLn $ "Day21: part1: " ++ show (S.size $ positions steps1 (sz,sz) cs)
  putStrLn $ "Day21: part2: " ++ show (a*k*k+b*k+c)

  return ()


test= [
    "..........."
  , ".....###.#."
  , ".###.##..#."
  , "..#.#...#.."
  , "....#.#...."
  , ".##..S####."
  , ".##..#...#."
  , ".......##.."
  , ".##.#.####."
  , ".##..##.##."
  , "..........."]
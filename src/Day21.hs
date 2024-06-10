module Day21(day21) where

import Utils
import Data.Set qualified as S
import Numeric.LinearAlgebra ( (<\>), (><), toList, fromList )
import Data.Array.Unboxed qualified as A


parse :: [String] -> A.UArray Coord Bool
parse xs = A.amap (`elem`  "S.") $ A.listArray ((0, 0), (length xs - 1, length (head xs) - 1)) (concat $ transpose xs)


{-# INLINE move #-}
move :: A.UArray Coord Bool -> Coord -> S.Set Coord
move mp p = S.fromList $ filter (\(x,y) -> mp A.! (x `mod` 131, y `mod` 131)) $ neighbours4 p


times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x
{-# INLINE times #-}


fitQuadratic :: [Double] -> [Double] -> (Int, Int, Int)
fitQuadratic xs ys = (round $ cs!!0, round $ cs!!1, round $ cs!!2)
  where
    cs = toList $ m <\> fromList ys
    m = (3><3) $ concatMap (\x -> [x*x, x, 1]) xs


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let !mp = parse ss
      steps1 = 64
      steps2 = 26501365 -- 202300*131+65
      k, sz :: Int
      (k, sz) = steps2 `divMod` 131 -- k == 202300, sz == 65
      start = S.singleton (sz,sz)
           
      c1,c2,c3 :: S.Set Coord
      c1 = times 65 (S.unions . S.map (move mp)) $ S.singleton (sz,sz)
      c2 = times 131 (S.unions . S.map (move mp)) c1
      c3 = times 131 (S.unions . S.map (move mp)) c2

      (a,b,c) = fitQuadratic [0,1,2] (fromIntegral . length <$> [c1,c2,c3])

  putStrLn $ "Day21: part1: " ++ show (length $ times steps1 (S.unions . S.map (move mp)) start)
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
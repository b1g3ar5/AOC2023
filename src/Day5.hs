{-# OPTIONS_GHC -Wno-orphans #-}

module Day5(day5) where

import Utils ( splitOn, getLines, chunksOf )
import Data.Set (Set)
import Data.Set qualified as S
import Data.Monoid
import Data.Semigroup


-- Map described by (range start, range end, increment)
type Map = Set (Int, Int, Int)

parseLine :: String -> (Int, Int, Int)
parseLine ls = (xs!!1, xs!!1 + xs!!2, xs!!0 - xs!!1)
  where
    xs = read <$> words ls


parseMap :: [String] -> Map
parseMap = S.fromList . (parseLine <$>) . drop 1


parseSeeds :: String -> [Int]
parseSeeds = (read <$>). drop 1 . words


parse :: String -> ([Int], [Map])
parse s = (parseSeeds $ ls!!0, parseMap <$> fs)
  where
    ls = lines s
    fs = splitOn "" $ drop 2 ls


-- Apply the map to a single seed
apply1' :: Int -> Map -> Int
apply1' seed map'
  | S.null map' = seed
  | nxt == seed = apply1' seed ms
  | otherwise = nxt
  where
    (m, ms) = S.deleteFindMin map'
    nxt = go seed m
    go s (lo, hi, inc)
      | s>=lo && s<hi = s+inc
      | otherwise = s


-- Apply the Map to a set of seed ranges
apply1 :: Set (Int, Int) -> Map -> Set (Int, Int)
apply1 = go S.empty
  where
    go :: Set (Int, Int) -> Set (Int, Int) -> Map -> Set (Int, Int)
    go acc seeds maps
      | S.null seeds = acc
      | S.null maps = acc <> seeds
      | hiSeed < mapStart = go (acc <> S.singleton (loSeed, hiSeed)) remainingSeeds maps -- Seed range too low for map - add to acc as it is
      | loSeed >= mapEnd = go acc seeds ms -- Seed range too high for map - drop the map
      | loSeed < min hiSeed mapStart && max loSeed mapEnd < hiSeed = go (acc <> S.singleton (inc + max loSeed mapStart, inc + min hiSeed mapEnd)) (remainingSeeds <> S.fromList [(loSeed, min hiSeed mapStart), (max loSeed mapEnd, hiSeed)]) ms
      | loSeed < min hiSeed mapStart = go (acc <> S.fromList [(loSeed,min hiSeed mapStart), (inc + max loSeed mapStart, inc + min hiSeed mapEnd)]) remainingSeeds maps
      | max loSeed mapEnd < hiSeed = go (acc <> S.singleton (inc + max loSeed mapStart, inc + min hiSeed mapEnd)) (remainingSeeds <> S.singleton (max loSeed mapEnd, hiSeed)) maps
      | otherwise = go (acc <> S.singleton (inc + max loSeed mapStart, inc + min hiSeed mapEnd)) remainingSeeds maps
      where
        ((loSeed, hiSeed), remainingSeeds) = S.deleteFindMin seeds
        ((mapStart, mapEnd, inc), ms) = S.deleteFindMin maps


day5 :: IO ()
day5 = do
  ls <- getLines 5
  let (seeds, maps) = parse $ unlines ls
      -- Make the seed ranges, they will be in order as Set is sorted (and I put rangeLo first)
      seeds2 :: Set (Int, Int)
      seeds2 = S.fromList $ concatMap (\s -> [(s!!0, s!!0 + s!!1)]) $ chunksOf 2 seeds

  putStrLn $ "Day5: part1: " ++ show (minimum $ (\s -> foldl apply1' s maps) <$> seeds)
  putStrLn $ "Day5: part1:1 " ++ show (fst $ S.findMin $ foldl apply1 seeds2 maps)

  return ()


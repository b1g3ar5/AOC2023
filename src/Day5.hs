{-# OPTIONS_GHC -Wno-orphans #-}

module Day5(day5) where

import Utils ( splitOn, getLines, chunksOf, timeIt )
import Data.Set (Set)
import Data.Set qualified as S


-- Map described by (range start, range end, increment)
type Range = (Int, Int, Int)
type Map = Set Range
-- A seed range
type R = (Int, Int)


isValid :: R -> Bool
isValid (l,h) = h>l


parseLine :: String -> Range
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


apply :: Int -> Map -> Int
apply seed map'
  | S.null map' = seed
  | nxt == seed = apply seed ms
  | otherwise = nxt
  where
    (m, ms) = S.deleteFindMin map'
    nxt = go seed m
    go s (lo, hi, inc)
      | s>=lo && s<hi = s+inc
      | otherwise = s


-- Only insert if the R is valid
vadd :: R -> Set R -> Set R
vadd r s
  | isValid r = r `S.insert` s
  | otherwise = s


-- Apply the Map to a set of seed ranges - splitting the range up where necessary
-- This relys on the Sets being sorted (ie. deleteFindMin is used).
applyR :: Set R -> Map -> Set R
applyR seeds maps 
  | S.null seeds = S.empty
  | S.null maps = seeds
  | loSeed >= mapEnd = applyR seeds ms -- current map is too low, discard it, inside will be invalid
  | hiSeed < mapStart = (loSeed, hiSeed) `S.insert` applyR remainingSeeds maps -- seeds below the maps
  -- The main case - add before and inside to the output, add after to the remainingSeeds and loop
  | otherwise = before `vadd` (inside `vadd` applyR (after `vadd` remainingSeeds) maps)
  where
    ((loSeed, hiSeed), remainingSeeds) = S.deleteFindMin seeds
    ((mapStart, mapEnd, inc), ms) = S.deleteFindMin maps
    before, inside, after :: R
    before = (loSeed,min hiSeed mapStart)
    inside = (inc + max loSeed mapStart, inc + min hiSeed mapEnd)
    after = (max loSeed mapEnd, hiSeed)


day5 :: IO ()
day5 = do
  ls <- getLines 5
  let (seeds, maps) = parse $ unlines ls
      -- Make the seed ranges, they will be in SORTED as I put the lo first (and it's a Set)
      seeds2 :: Set R
      seeds2 = S.fromList $ concatMap (\s -> [(s!!0, s!!0 + s!!1)]) $ chunksOf 2 seeds

  timeIt $ putStrLn $ "Day5: part1: " ++ show (minimum $ (\s -> foldl apply s maps) <$> seeds)
  timeIt $ putStrLn $ "Day5: part2: " ++ show (fst $ S.findMin $ foldl applyR seeds2 maps)

  return ()


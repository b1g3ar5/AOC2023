module Day12(day12) where

import Utils (intercalate, getLines, wordsBy, bimap)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Function (fix)
import Memo2 qualified as M2
import Numeric.Natural


parse1 :: String -> (String, [Int])
parse1 s = (ws!!0, read <$> wordsBy (==',') (ws!!1))
  where
    ws = words s


parse2 :: String -> (String, [Int])
parse2 s = bimap (intercalate "?" . replicate 5) (concat . replicate 5) $ parse1 s


countF :: ((String, [Int]) -> Int) -> (String, [Int]) -> Int
countF _ ([], []) = 1
countF _ ([], _)  = 0
countF _ (cfg, []) = if '#' `elem` cfg then 0 else 1
countF recurFn (cfg@(c:cs), s:ss)
  | c == '.' = recurFn (cs, s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg!!s /= '#' ) = recurFn (drop (s+1) cfg, ss)
  | c == '?' = recurFn ('.':cs, s:ss ) + recurFn ('#':cs, s:ss)
  | otherwise = 0


count :: (String, [Int]) -> Int
count = fix countF


encodeSI :: (String, [Int]) -> Natural
encodeSI = undefined
decodeSI :: Natural -> (String, [Int])
decodeSI = undefined


countED :: (Natural -> Int) -> (Natural -> Int)
countED f = f . encodeSI . decodeSI



--countTree :: (String, [Int]) -> Int
countTree = M2.memoize @M2.Tree countED


fibOp :: (Natural -> Integer) -> (Natural -> Integer)
fibOp v 0 = 0
fibOp v 1 = 1
fibOp v n = v (n-1) + v (n-2)


fibTree :: Natural -> Integer
fibTree = M2.memoize @M2.Tree fibOp



type Cache = Map (String, [Int]) Int

-- Self memoed
count2 :: Cache -> String -> [Int] -> (Int, Cache)
count2 seen [] [] = (1, seen)
count2 seen [] _  = (0, seen)
count2 seen springs [] = if '#' `elem` springs then (0, seen) else (1, seen)
count2 seen springs@(c:cs) scores@(s:ss)
  | (springs, scores) `M.member` seen = (seen M.! (springs, scores), seen)
  | c == '.' = (nextCount, M.insert (cs, s:ss) nextCount nextSeen)
  | c == '#' && (s <= length springs) && ('.' `notElem` take s springs) && (s == length springs || springs!!s /= '#' ) = (dropCount, M.insert (drop (s+1) springs, ss) dropCount dropSeen)
  | c == '?' = (dotCount + hashCount, M.insert ('#':cs, s:ss) hashCount hashSeen)
  | otherwise = (0, seen)
    where
      -- for c=='.'
      (nextCount, nextSeen) = count2 seen cs (s:ss)
      -- for c=='#' - here we drop all the rest of the 'word'
      (dropCount, dropSeen) = count2 seen (drop (s+1) springs) ss
      -- for c=='?' - just replace with a '.' or a '#'
      (dotCount, dotSeen) = count2 seen ('.':cs) (s:ss)
      (hashCount, hashSeen) = count2 (M.insert ('.':cs, s:ss) dotCount dotSeen) ('#':cs) (s:ss)


day12 :: IO ()
day12 = do
  ls <- getLines 12
  let g1 = parse1 <$> ls
      g2 = parse2 <$> ls

  putStrLn $ "Day12: part1: " ++ show (sum $ count <$> g1) 
  putStrLn $ "Day12: part2: " ++ show (sum $ fst . uncurry (count2 M.empty) <$> g2)


  return ()

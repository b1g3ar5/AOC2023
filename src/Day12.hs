module Day12(day12) where

import Utils (intercalate, getLines, wordsBy, bimap)
import Control.Monad.Memo


parse :: String -> (String, [Int])
parse s
  | null ws = ([], [])
  | length ws < 2 = if head s ==' ' then ("", read <$> wordsBy (==',') (ws !! 0)) else (ws !! 0, [])
  | otherwise = (ws !! 0, read <$> wordsBy (==',') (ws !! 1))
  where
    ws = words s


parse2 :: String -> (String, [Int])
parse2 s = bimap (intercalate "?" . replicate 5) (concat . replicate 5) $ parse s


countM :: (MonadMemo (String, [Int]) Int m) => (String, [Int]) -> m Int
countM ([], ss)  = if null ss then return 1 else return 0
countM (cfg, []) = if '#' `elem` cfg then return 0 else return 1
countM (cfg@(c:cs), s:ss)
  | c == '.' = memo countM (cs, s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg !! s /= '#' ) = memo countM (drop (s+1) cfg, ss)
  | c == '?' = do 
                x1 <- memo countM ('.':cs, s:ss)
                x2 <- memo countM ('#':cs, s:ss)
                return $ x1+x2
  | otherwise = return 0


evalCount :: (String, [Int]) -> Int
evalCount = startEvalMemo . countM


day12 :: IO ()
day12 = do
  ls <- getLines 12
  let g1 = parse <$> ls
      g2 = parse2 <$> ls

  putStrLn $ "Day12: part1: " ++ show (sum $ evalCount <$> g1)
  putStrLn $ "Day12: part2: " ++ show (sum $ evalCount <$> g2)

  return ()




{- 

####################### GRAVEYARD #################################

import Data.Map (Map)
import Data.Map qualified as M
import Data.Function (fix)

countF :: ((String, [Int]) -> Int) -> (String, [Int]) -> Int
countF _ ([], ss)  = if null ss then 1 else 0
countF _ (cfg, []) = if '#' `elem` cfg then 0 else 1
countF recurFn (cfg@(c:cs), s:ss)
  | c == '.' = recurFn (cs, s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg `ix` s /= '#' ) = recurFn (drop (s+1) cfg, ss)
  | c == '?' = recurFn ('.':cs, s:ss ) + recurFn ('#':cs, s:ss)
  | otherwise = 0

count :: (String, [Int]) -> Int
count = fix countF



type Cache = Map (String, [Int]) Int

cache :: Map (String, [Int]) Int
cache = M.empty


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




-}
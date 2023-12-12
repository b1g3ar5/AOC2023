module Day12(day12) where

import Utils (intercalate, getLines, wordsBy, bimap)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Function.Memoize

parse1 :: String -> (String, [Int])
parse1 s = (ws!!0, read <$> wordsBy (==',') (ws!!1))
  where
    ws = words s


parse2 :: String -> (String, [Int])
parse2 s = bimap (intercalate "?" . replicate 5) (concat . replicate 5) $ parse1 s


count1 :: String -> [Int] -> Int
count1 [] [] = 1
count1 [] _  = 0
count1 cfg [] = if '#' `elem` cfg then 0 else 1
count1 cfg@(c:cs) (s:ss)
  | c == '.' = count1 cs (s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg!!s /= '#' ) = count1 (drop (s+1) cfg) ss
  | c == '?' = count1 ('.':cs) (s:ss ) + count1 ('#':cs) (s:ss)
  | otherwise = 0


rcount1 :: (String -> [Int] -> Int) -> String -> [Int] -> Int
rcount1 f [] [] = 1
rcount1 f [] _  = 0
rcount1 f cfg [] = if '#' `elem` cfg then 0 else 1
rcount1 f cfg@(c:cs) (s:ss)
  | c == '.' = rcount1 f cs (s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg!!s /= '#' ) = rcount1 f (drop (s+1) cfg) ss
  | c == '?' = rcount1 f ('.':cs) (s:ss ) + rcount1 f ('#':cs) (s:ss)
  | otherwise = 0


memoCount1 = memoFix2 rcount1


type Cache = Map (String, [Int]) Int

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

  putStrLn $ "Day12: part1: " ++ show (sum $ uncurry count1 <$> g1) 
  putStrLn $ "Day12: part2: " ++ show (sum $ fst . uncurry (count2 M.empty) <$> g2)
  --putStrLn $ "Day12: part1: " ++ show (sum $ uncurry memoCount1 <$> g1) 
  --putStrLn $ "Day12: part1: " ++ show (sum $ uncurry memoCount1 <$> g2) 
  return ()



data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)


-- Function to memoise
testF :: (Integer -> Integer) -> Integer -> Integer
testF _ 0 = 0
testF mf n = max n $ mf (n `div` 2) +
                 mf (n `div` 3) +
                 mf (n `div` 4)


-- Tree of the answers
fTree :: Tree Integer
fTree = fmap (testF fastestF) nats


-- Fast version of testF
fastestF :: Integer -> Integer
fastestF = index fTree



nats :: Tree Integer
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2


index :: Tree a -> Integer -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
    (q,0) -> index l q
    (q,1) -> index r q



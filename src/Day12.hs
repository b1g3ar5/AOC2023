module Day12(day12) where

import Utils (intercalate, getLines, wordsBy, bimap)
import Data.List 
import Data.Map (Map)
import Data.Map qualified as M
import Data.Function (fix)
import Memo2 qualified as M2
import Numeric.Natural ( Natural )
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as C
import qualified Data.Bits.Utils as C
import Data.Binary qualified as B
import qualified Data.Bits.Utils as B
import Data.Text.Encoding (encodeUtf8)
import Data.Bits ( Bits((.|.), shiftR, shiftL) )
import Data.Bits.Utils ( c2w8 )
import Data.Vector.Fusion.Bundle.Monadic (Bundle(sSize))
import Memo qualified as Memo


unroll :: Integer -> [B.Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)


roll :: [B.Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b



encode :: (String, [Int]) -> Integer
encode si = roll $ c2w8 <$> showSpring si


decode :: Integer -> (String, [Int])
decode x = parse $ B.w82s $ unroll x



parse :: String -> (String, [Int])
parse s 
  | null ws = ([], []) --error $ "Error in parse: " ++ s
  | length ws < 2 = (['#'], []) --error $ "Error in parse: " ++ s
  | otherwise = (ws `ix` 0, read <$> wordsBy (==',') (ws `ix` 1))
  where
    ws = words s


showSpring :: (String, [Int]) -> String
showSpring (s, xs) = s ++ " " ++ intercalate "," (show <$> xs)

parse2 :: String -> (String, [Int])
parse2 s = bimap (intercalate "?" . replicate 5) (concat . replicate 5) $ parse s


countF :: ((String, [Int]) -> Int) -> (String, [Int]) -> Int
countF _ ([], []) = 1
countF _ ([], _)  = 0
countF _ (cfg, []) = if '#' `elem` cfg then 0 else 1
countF recurFn (cfg@(c:cs), s:ss)
  | c == '.' = recurFn (cs, s:ss)
  | c == '#' && (s <= length cfg) && ('.' `notElem` take s cfg) && (s == length cfg || cfg `ix` s /= '#' ) = recurFn (drop (s+1) cfg, ss)
  | c == '?' = recurFn ('.':cs, s:ss ) + recurFn ('#':cs, s:ss)
  | otherwise = 0


ix :: [a] -> Int -> a
ix xs i
  | i>(length xs-1) = error $ "i is too big: " ++ show i
  | otherwise = xs !!i


countFI :: (Integer -> Int) -> Integer -> Int
countFI f i = countF rf $ decode i
  where
    rf :: (String, [Int]) -> Int
    rf si = f (encode si)


-- Build a tree
countTree :: Memo.Tree Int
countTree = fmap (countFI fastCount) Memo.nats


-- Get the answer from the tree
fastCount :: Integer -> Int
fastCount = Memo.index countTree


count' :: (String, [Int]) -> Int
count' ss = fastCount $ encode ss


count :: (String, [Int]) -> Int
count = fix countF


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
  let g1 = parse <$> ls
      g2 = parse2 <$> ls
      
  putStrLn $ "Day12: part1: " ++ show (sum $ count <$> g1) 
  putStrLn $ "Day12: part2: " ++ show (sum $ fst . uncurry (count2 M.empty) <$> g2)
  --putStrLn $ "Day12: part2: " ++ show (g2!!0)
  --putStrLn $ "Day12: part2: " ++ show (encode $ g2!!0)
  --putStrLn $ "Day12: part2: " ++ show (decode $ encode $ g2!!0)
  putStrLn $ "Day12: part2:count " ++ show (count ("#.??", [1]))
  putStrLn $ "Day12: part2:count " ++ show (count (".??", [0]))
  putStrLn $ "Day12: part2:count " ++ show (count ("??", [0]))
  putStrLn $ "Day12: part2:count' " ++ show (count' ("#.??", [1]))
  putStrLn $ "Day12: part2:count' " ++ show (count' (".??", [0]))
  putStrLn $ "Day12: part2:count' " ++ show (count' ("??", [0]))
  

  return ()


-- fib0
-- fib1 = fib0 with recursion removed
-- fibTree = build tree

{-

-- ############# Example #############
fib0 :: Integer -> Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n-1) + fib0 (n-2)



-- Remove the recursion
fib1 :: (Integer -> Integer) -> Integer -> Integer
fib1 f 0 = 0
fib1 f 1 = 1
fib1 f n = f (n-1) + f (n-2)


-- Build a tree
fibTree :: Tree Integer
fibTree = fmap (fib1 fastestFib) nats


-- Get the answer from the tree
fastestFib :: Integer -> Integer
fastestFib = index fibTree



-}
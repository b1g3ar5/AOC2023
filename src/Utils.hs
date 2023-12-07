{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Utils (
  getLines
  , splitOn
  , clockTurn
  , antiTurn
  , splitWhen
  , chunksOf
  , parseWith
  , many
  , many1
  , sepBy
  , sepBy1
  , satisfy
  , pInt
  , digit
  , isDigit
  , Coord
  , Coord3
  , scale3
  , scale
  , neighbours4
  , neighbours6
  , neighbours8
  , nextTo8
  , euclidian
  , manhattan
  , manhattan3
  , lt, rt, up, dn
  , lt3, rt3, up3, dn3, in3, ot3
  , leftOf, rightOf, above, below
  , directions4
  , directions8
  , fromJust
  , fromMaybe
  , sort
  , group
  , groupBy
  , sortBy 
  , sortOn
  , elemIndex 
  , findIndex 
  , nub
  , intercalate
  , transpose
  , ord
  , chr
  , bimap 
  , first
  , second
  , on
  , lefts
  , rights
  , timeIt
  , isJust
  , isNothing
  , trace
  , floodFill
  , bfs
  , fromRight
  , catMaybes
  , mapMaybe
  , comparing
  , levi
  , crossProduct
  , traverse_
  , ReadP
  , parseS
  , string
  , swap
  , parseGridWith
) where

import Data.Char ( ord, isDigit, isSpace, chr )
import Data.Tuple (swap)
import Data.List.Split (chunksOf)
import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing, catMaybes, mapMaybe )
import Data.List ( elemIndex, findIndex, group, groupBy, sort, sortBy, sortOn, nub, intercalate, transpose ) 
import Data.Bifunctor ( Bifunctor(second, bimap, first) )
import Data.Function ( on )
import Data.Either ( lefts, rights, fromRight )
import System.TimeIt ( timeIt )
import Text.ParserCombinators.ReadP ( ReadP, many1, readP_to_S, satisfy, string, many, sepBy, sepBy1 )
import Data.Hashable ( Hashable )
import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Queue as Q
import Data.Foldable ( Foldable(foldl'), traverse_ )
import Data.Ord ( comparing )



--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".in"
  return $ f s


getT :: (String -> a) -> Int -> IO a
getT f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".ex"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getTest :: Int -> IO [String]
getTest = getT lines


-- splits a list on an item and deletes the item
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s = go [] []
  where
    go acc next [] = acc ++ [next]
    go acc next (l:ls)
      | l == s = go (acc ++ [next]) [] ls
      | otherwise = go acc (next ++ [l]) ls


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p = go [] []
  where
    go acc next [] = acc ++ [next]
    go acc next (l:ls)
      | p l = go (acc ++ [next]) [] ls
      | otherwise = go acc (next ++ [l]) ls


parseS :: ReadP a -> ReadS a
parseS = readP_to_S


parseWith :: ReadP a -> String -> a
parseWith p s = case [ a | (a,t) <- parseS p s, all isSpace t] of
                  [a] -> a
                  [] -> error "No parse"
                  _ -> error "Ambiguous parse"


digit :: ReadP Char
digit = satisfy (`elem` "0123456789")

pInt :: ReadP Int
pInt = do
  n <- many1 digit
  return $ read n


------------------ VARIOUS UTILITY FUNCTIONS --------------------


binSearch :: Integral t => (t -> Bool) -> t -> t -> t
binSearch p = go
  where
    go lo hi
      | lo + 1 == hi = hi
      | p mid = go lo mid
      | otherwise = go mid hi
      where
        mid = (lo + hi) `div` 2


steadyState :: Eq a => (a -> a) -> a -> a
steadyState f x = if f x == x then x else steadyState f (f x)


steadyState' :: Eq a => (a -> Maybe a) -> a -> Maybe a
steadyState' f x = case f x of
                     Nothing -> Just x
                     Just y -> steadyState' f y



------------------------ COORDINATE / VECTOR STUFF ------------


type Coord = (Int, Int)
type Coord3 = (Int, Int, Int)

instance {-# OVERLAPPING #-} Hashable Coord where

instance Num Coord where
  {-# INLINE (+) #-}
  (+), (-), (*) :: Coord -> Coord -> Coord
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs, signum :: Coord -> Coord
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger :: Integer -> Coord
  fromInteger i = (fromInteger i, 0)


instance Num Coord3 where
  (x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
  (x1, y1, z1) - (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
  (x1, y1, z1) * (x2, y2, z2) = (x1*x2, y1*y2, z1*z2)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger i = (fromInteger i, 0, 0)


scale3 :: Int -> Coord3 -> Coord3
scale3 k (x,y,z) = (k*x, k*y, k*z)

scale :: Int -> Coord -> Coord
scale k (x,y) = (k*x, k*y)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords4 :: [Coord]
neighbourCoords4 = [(1,0), (-1,0), (0,1), (0,-1)]

neighbourCoords6 :: [Coord3]
neighbourCoords6 = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(-1, -1),(0, -1),(1, -1), (1, 0),(1, 1),(0, 1),(-1, 1),(-1, 0)]


neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c

neighbours6 :: Coord3 -> [Coord3]
neighbours6 c = neighbourCoords6 `at3` c


{-# INLINE neighbours8 #-}
neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c

{-# INLINE nextTo8 #-}
nextTo8 :: Coord -> Coord -> Bool
nextTo8 p q = p `elem` neighbours8 q

{-# INLINE at #-}
at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords

at3 :: [Coord3] -> Coord3 -> [Coord3]
coords `at3` origin = map (+ origin) coords


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concatMap (\c -> (c,) <$> [0..(rows-1)]) [0..(cols-1)]


directions8 :: [Coord]
directions8 = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]


directions4 :: [Coord]
directions4 = [(0, -1), (0, 1), (1, 0), (-1, 0)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)


lt, rt, up, dn :: Coord
lt = (-1,0)
rt = (1,0)
up = (0,-1)
dn = (0,1)

lt3, rt3, up3, dn3, in3, ot3 :: Coord3
lt3 = (-1,0,0)
rt3 = (1,0,0)
up3 = (0,-1,0)
dn3 = (0,1,0)
in3 = (0,0,1)
ot3 = (0,0,-1)

levi :: Int -> Int -> Int -> Int
levi 0 1 2 = 1
levi 1 2 0 = 1
levi 2 0 1 = 1
levi 0 2 1 = -1
levi 2 1 0 = -1
levi 1 0 2 = -1
levi i j k
  | i==j || i == k || j == k = 0
  | otherwise = error $ "Error in Levi Civita for i,j,k: " ++ show (i,j,k)


parseGridWith :: (Char -> a) -> [String] -> [(Coord, a)]
parseGridWith  p css = sym
  where
    sym = concatMap (\(y, cs) -> (\(x, c) -> ((x,y), p c)) <$> zip [0..] cs) $ zip [0..] css


-- Basis...
e :: [Coord3]
e = [(1,0,0), (0,1,0), (0,0,1)]

crossProduct :: Coord3 -> Coord3 -> Coord3
crossProduct (v1,v2,v3) (w1,w2,w3) = sum $ (\i -> sum $ (\j -> sum $ (\k -> scale3 (levi i j k * (v!!j) * (w!!k)) (e!!i)) <$> [0..2]) <$> [0..2]) <$> [0..2]
  where
    v = [v1,v2,v3]
    w = [w1,w2,w3]



floodFill :: Ord a => a -> (a -> [a]) -> [a]
floodFill start getNext = go Set.empty (Q.fromList [start]) --(Seq.singleton start)
  where
    go !seen = \case
                Q.Empty -> []
                x Q.:<| newq 
                  | x `Set.member` seen -> go seen newq
                  | otherwise -> x : go (x `Set.insert` seen) (Q.appendList newq (getNext x))
  

bfs :: Ord a => (a -> [a]) -> [a]-> [a]
bfs next start = loop Set.empty (Q.fromList start)
  where
    loop !seen = \case
                  Q.Empty -> []
                  x Q.:<| newq
                    | x `Set.member` seen -> loop seen newq
                    | otherwise -> x : loop (x `Set.insert` seen) (Q.appendList newq (next x))


dfs :: Ord a => (a -> [a]) -> [a]-> [a]
dfs next start = loop Set.empty (Q.fromList start)
  where
    loop !seen = \case
                  Q.Empty -> []
                  x Q.:<| newq
                    | x `Set.member` seen -> loop seen newq
                    | otherwise -> x : loop (x `Set.insert` seen) (foldl' (\q x -> Q.cons x q) newq (next x))



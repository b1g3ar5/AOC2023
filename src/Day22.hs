{-# OPTIONS_GHC -Wno-orphans #-}
module Day22(day22) where

import Utils
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.List ( delete )


parse :: String -> (Coord3, Coord3)
parse s = ((read $ c1!!0, read $ c1!!1, read $ c1!!2), (read $ c2!!0, read $ c2!!1, read $ c2!!2))
  where
    ws = splitOn '~' s
    c1 = wordsBy (==',') $ ws!!0
    c2 = wordsBy (==',') $ ws!!1


unit :: Coord3 -> Coord3 -> Coord3
unit v1 v2
  | x==0 && y==0 = (0,0,1)
  | x==0 && z==0 = (0,1,0)
  | y==0 && z==0 = (1,0,0)
  | otherwise = error $ "Not a carinial direction: " ++ show v1 ++ ", " ++ show v2
  where
    (x,y,z) = v1 - v2


brick :: (Coord3, Coord3) -> [Coord3]
brick (st, en) = (\i -> st + i `scale3` unit st en) <$> [0..(manhattan3 st en)]


type Brick = [Coord3]

instance {-#overlaps#-} Ord Coord3 where
  (x1, y1, z1) <= (x2, y2, z2)
    | z1 == z2 && y1 == y2 = x1 <= x2
    | z1 == z2 = y1 <= y2
    | otherwise = z1 <= z2


getZ :: Coord3 -> Int
getZ (_,_,z) = z


lowestPoint :: Brick -> Int
lowestPoint cs = minimum $ getZ <$> cs


fall1, support :: Brick -> Brick
fall1 b = if lowestPoint b > 1 then (\(x,y,z) -> (x,y,z-1)) <$> b else b
support b = filter (`notElem` b) $ fall1 b


-- Bricks must be sorted before calling...
fall :: [Brick] -> [Brick]
fall = fst . foldl go ([], S.empty)
  where
    go :: ([Brick], Set Coord3) -> Brick -> ([Brick], Set Coord3)
    go (acc, occupied) b
      | newb == b = (acc ++ [b], S.union occupied $ S.fromList b)
      | overlaps = (acc ++ [b], S.union occupied $ S.fromList b)
      | otherwise = go (acc, occupied) newb
      where
        newb = fall1 b
        overlaps = any (`S.member` occupied) newb


supportedBy :: [Brick] -> Map Int [Int]
supportedBy bricks = M.fromList $ go <$> zip [0..] bricks
  where
    go :: (Int, Brick) -> (Int, [Int])
    go (ix, b) = (ix, mapMaybe (\(jx, cs) -> if any (`S.member` supportPoints) cs then Just jx else Nothing) (zip [0..] bricks))
      where
        supportPoints = S.filter (`notElem` b) $ S.fromList $ support b


countFallers :: Map Int [Int] -> Int -> Int
countFallers brickMap iix = (-1) + S.size (go brickMap (S.singleton iix) (S.singleton iix))
  where  
    go :: Map Int [Int] -> Set Int -> Set Int -> Set Int
    go mp pipeline fallers 
      | null pipeline = fallers
      -- | otherwise = go rs (newPipeline `S.union` S.fromList (filter (`S.notMember` fallers) $ M.keys fs)) (fallers `S.union` S.fromList (M.keys fs))
      | otherwise = go rs (newPipeline `S.union` S.fromList (M.keys fs)) (fallers `S.union` S.fromList (M.keys fs))
      where
        (ix, newPipeline) = S.deleteFindMin pipeline
        (fs, rs) = M.partition (==[]) $ delete ix <$> mp 


day22 :: IO ()
day22 = do
  ss <- getLines 22
  --let ss = test
  let bs = S.fromList $ brick . parse <$> ss
      fallen = fall $ sortOn lowestPoint $ S.toList bs
      supportMap = supportedBy fallen
      disintegrationMap = filter (\ix -> null $ M.filter (\e -> ix `elem` e && length e == 1) supportMap) [0..(length fallen - 1)]

  putStrLn $ "Day22: part2: " ++ show (length disintegrationMap) -- 505
  putStrLn $ "Day22: part2: " ++ show (sum $ countFallers (M.filter (/=[]) supportMap) <$> [0..(length bs-1)]) -- 71002

  return ()


test = ["1,0,1~1,2,1"
  , "0,0,2~2,0,2"
  , "0,2,3~2,2,3"
  , "0,0,4~0,2,4"
  , "2,0,5~2,2,5"
  , "0,1,6~2,1,6"
  , "1,1,8~1,1,9"]
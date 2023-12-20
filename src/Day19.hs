{-# OPTIONS_GHC -Wno-orphans #-}
module Day19(day19) where

import Utils (getLines, hylo, splitOn)
import Data.Map qualified as M


-- Over doing the types here...
type Name = String
type AttributeIx = Int
type Item = [Int] -- an object has 4 properties - in a list
type Condition = (AttributeIx, Int -> Bool, String, Int, Name)
type Workflows = M.Map Name ([Condition], Name)
type Range = (Int, Int) -- range of a property
type Ranges = [Range] -- ranges of all the poreperties


-- For part 1
apply :: Workflows -> Item -> String
apply wfs i = go "in"
  where
    go name
      | name == "A" = "A"
      | name == "R" = "R"
      | otherwise = go $ gg cds
        where
          (cds, ow) = wfs M.! name
          gg [] = ow
          gg ((aa, op, _, _, nm):cs) =  if op (i !! aa) then nm else gg cs


-- NullF for 'R', LeafF for 'A', NodeF for the rest
data BTreeF a r = NullF | LeafF a | NodeF r r deriving (Functor)
data BTreeF' a r = NullF' | LeafF' | NodeF' a r r deriving (Functor)


-- Build up the tree from the map.
coalg :: (Workflows, Name, Ranges) -> BTreeF Ranges (Workflows, Name, Ranges)
coalg (mp, name, ranges)
  | name == "R" = NullF
  | name == "A" = LeafF ranges
  | length cs == 1 = NodeF (mp, cname c, trueR) (mp, ow, falseR)
  | otherwise = NodeF (mp, cname c, trueR) (M.insert name (tail cs,ow) mp, name, falseR)
  where
    (cs, ow)  = mp M.! name
    c = head cs -- There is a null cs check!
    (trueR, falseR) = splitRange (head cs) ranges 


alg :: BTreeF Ranges Int -> Int
alg NullF = 0 
alg (LeafF rs) = product $ (\(l,h) -> h-l+1) <$> rs
alg (NodeF l r) = l + r


coalg' :: (Workflows, Name) -> BTreeF' Condition (Workflows, Name)
coalg' (mp, name)
  | name == "R" = NullF'
  | name == "A" = LeafF'
  | length cs == 1 = NodeF' c (mp, cname c) (mp, ow)
  | otherwise = NodeF' c (mp, cname c) (M.insert name (tail cs,ow) mp, name)
  where
    (cs, ow)  = mp M.! name
    c = head cs -- There is a null cs check!


alg' :: BTreeF' Condition (Ranges -> Int) -> (Ranges -> Int)
alg' NullF' = const 0
alg' LeafF' = score
alg' (NodeF' c l r) = \rs -> l (reduce c rs) + r (reduce (opposite c) rs)


splitRange :: Condition -> Ranges -> (Ranges, Ranges)
splitRange c rs = (reduce c rs, reduce (opposite c) rs)


-- Reduce a set of ranges by a condition (only one gets reduced)
reduce :: Condition -> [(Int, Int)] -> [(Int, Int)]
reduce (ix, _, c, x, _) rs
  | c==">" = (\(i,(l,h)) ->  if i==ix then (max l (x+1), h) else (l,h)) <$> zip [0..] rs
  | c==">=" = (\(i,(l,h)) -> if i==ix then (max l x, h) else (l,h)) <$> zip [0..] rs
  | c=="<" = (\(i,(l,h)) ->  if i==ix then (l, min (x-1) h) else (l,h)) <$> zip [0..] rs
  | c=="<=" = (\(i,(l,h)) -> if i==ix then (l, min x h) else (l,h)) <$> zip [0..] rs
  | otherwise = rs


score :: Ranges -> Int
score rs = product $ (\(l,h) -> h-l+1) <$> rs


day19 :: IO ()
day19 = do
  ss <- getLines 19
  let (workflows, ps) = parse ss
      start :: [(Int, Int)]
      start = replicate 4 (1,4000)

  putStrLn $ "Day19: part1: " ++ show (sum $ sum . fst <$> filter ((=="A"). snd) ((\p -> (p, apply workflows p)) <$> ps))
  putStrLn $ "Day19: part2: " ++ show (hylo alg coalg (workflows, "in", start))
  putStrLn $ "Day19: part2: " ++ show (hylo alg' coalg' (workflows, "in") start)
  
  return ()


-- THE (COMPLICATED) PARSING ---



cname :: Condition -> Name
cname (_,_,_,_,n) = n


opposite :: Condition -> Condition
opposite (a,f,c,x,n)
  | c==">" = (a, not . f, "<=", x, n )
  | c==">=" = (a, not . f, "<", x, n )
  | c=="<" = (a, not . f, ">=", x, n )
  | c==">=" = (a, not . f, "<", x, n )
  | otherwise = error "Error in the opp function day 19"


toIndex :: Char -> Int
toIndex 'x' = 0
toIndex 'm' = 1
toIndex 'a' = 2
toIndex 's' = 3
toIndex c = error "Not an attribute"


parseWorkflow :: String -> (Name, ([Condition], Name))
parseWorkflow s = (name, (parseCondition <$> init ps, last ps))
  where
    (name, remaining) = span (/='{') s
    ps = splitOn ',' $ init $ tail remaining


parseCondition :: String -> Condition
parseCondition s = (toIndex attribute, opp (read sn), [head $ tail s], read sn, tail name)
  where
    attribute = head s
    (sn, name) = span (/=':') $ tail $ tail s
    opp = op $ head $ tail s
    op '>' = (<)
    op '<' = (>)
    op c = error $ "Op must be [<>]: " ++ show [c]


parse :: [String] -> (Workflows, [Item])
parse ls = (M.fromList $ parseWorkflow <$> wfs, parseItem <$> tail is)
  where
    (wfs, is) = span (/="") ls


parseItem :: String -> Item
parseItem s = read . drop 2 <$> ws
  where
    ws = splitOn ',' $ init $ tail s


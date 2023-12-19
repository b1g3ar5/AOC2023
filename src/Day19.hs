{-# OPTIONS_GHC -Wno-orphans #-}
module Day19(day19) where

import Utils hiding (NodeF)
import Data.Map qualified as M


-- Over doing the types here...
type Name = String
type AttributeIx = Int
type Item = [Int] -- an object has 4 properties - in a list
type Condition = (AttributeIx, Int -> Bool, String, Int, Name)
type Workflows = M.Map Name ([Condition], Name)
type Range = (Int, Int) -- range of a property
type Ranges = [Range] -- ranges of all the poreperties


splitRange :: Condition -> Ranges -> (Ranges, Ranges)
splitRange c rs = (reduce c rs, reduce (opposite c) rs)


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



-- Reduce a set of ranges by a condition (only one gets reduced)
reduce :: Condition -> [(Int, Int)] -> [(Int, Int)]
reduce (ix, _, c, x, _) rs
  | c==">" = (\(i,(l,h)) ->  if i==ix then (max l (x+1), h) else (l,h)) <$> zip [0..] rs
  | c==">=" = (\(i,(l,h)) -> if i==ix then (max l x, h) else (l,h)) <$> zip [0..] rs
  | c=="<" = (\(i,(l,h)) ->  if i==ix then (l, min (x-1) h) else (l,h)) <$> zip [0..] rs
  | c=="<=" = (\(i,(l,h)) -> if i==ix then (l, min x h) else (l,h)) <$> zip [0..] rs
  | otherwise = rs


-- A recursive Binary Tree - to be fixed...
data BTreeF a r = LeafF a | NodeF r r deriving (Functor)


-- A bit tricky in the coalgebra because we have to take conditions out of the tree
-- Build up the tree from the map
coalg :: (Workflows, Name, Ranges) -> BTreeF Ranges (Workflows, Name, Ranges)
coalg (mp, name, ranges)
  | name == "A" = LeafF ranges
  | name == "R" = LeafF []
  | null cs = error "Yes, we have no conditions in coalg"
  | length cs == 1 = NodeF (mp, cname c, trueR) (mp, ow, falseR)
  | otherwise = NodeF (mp, cname c, trueR) (M.insert name (tail cs,ow) mp, name, falseR)
  where
    (cs, ow)  = mp M.! name
    c = head cs
    (trueR, falseR) = splitRange (head cs) ranges 


alg :: BTreeF Ranges Int -> Int
alg (LeafF []) = 0 -- because product of an empty list is 1
alg (LeafF rs) = product $ (\(l,h) -> h-l+1) <$> rs
alg (NodeF l r) = l + r


day19 :: IO ()
day19 = do
  ss <- getLines 19
  let (workflows, ps) = parse ss
      start :: [(Int, Int)]
      start = replicate 4 (1,4000)

  putStrLn $ "Day19: part1: " ++ show (sum $ sum . fst <$> filter ((=="A"). snd) ((\p -> (p, apply workflows p)) <$> ps))
  --putStrLn $ "Day19: part2: " ++ show (count workflows "in" start)
  putStrLn $ "Day19: part2: " ++ show (hylo alg coalg (workflows, "in", start))
  
  return ()



{- Other method with out hylo


count :: Workflows -> Name -> Ranges -> Int
-- If we get to an A it's just the product of the ranges
count _ "A" rs = product $ (\(l,h) -> h-l+1) <$> rs
-- If we get to a R nothing works
count _ "R" _ = 0
-- The recursive bit
count mp flowName ranges = go ranges css
  where
    -- Get he next set of conditions and the otherwise workflow
    (css, ow) = mp M.! flowName
    go ::  Ranges -> [Condition] -> Int
    go _ [] = error "How did we get here? As the days go by..."
    -- If ther's only one condition left for the false side call the otherwise workflow
    go rs [cnd] = count mp (cname cnd) rTrue + count mp ow rFalse
      where
        (rTrue, rFalse) = splitRange cnd rs
    -- For the true ranges count the workflow, for the false side call go with the remaining conditions
    go rs (cnd:cs) = count mp (cname cnd) rTrue + go rFalse cs
      where
        (rTrue, rFalse) = splitRange cnd rs
-}

module Day7(day7) where

import Utils ( sort, group, getLines, first, comparing )


-- 2 versions so I can have 2 Ord instances
type Card = Int
newtype Hand1 = H1 [Card] deriving (Eq)
newtype Hand2 = H2 [Card] deriving (Eq)


parseHand :: String -> (Hand1, Int)
parseHand s = (H1 $ readCard <$> ws!!0, read $ ws!!1)
  where
    ws = words s


readCard :: Char -> Card
readCard c
  | c=='T' = 10
  | c=='J' = jack
  | c=='Q' = 12
  | c=='K' = 13
  | c=='A' = 14
  | otherwise = read [c]


jack, joker :: Card
joker = 1
jack = 11


convert :: Hand1 -> Hand2
convert (H1 cs) = H2 $ (\c -> if c == jack then joker else c) <$> cs


data Rank = High | OneP | TwoP | Three | Full | Four | Five  deriving (Eq, Ord, Show)


rank1 :: Hand1 -> Rank
rank1 (H1 cs)
  | maxSize == 5 = Five
  | maxSize == 4 = Four
  | szs == [2,3] = Full
  | maxSize == 3 = Three
  | szs == [1,2,2] = TwoP
  | maxSize == 2 = OneP
  | otherwise = High
  where
    szs :: [Int]
    szs = sort $ (length <$>) <$> group $ sort cs
    maxSize :: Int
    maxSize = maximum szs
    

rank2 :: Hand2 -> Rank
rank2 (H2 cs)
  | jokers == 5 = Five
  | maxSize == 5 = Five
  | maxSize == 4 = Four
  | szs == [2,3] || szs == [2,2] = Full
  | maxSize == 3 = Three
  | szs == [1,2,2] = TwoP
  | maxSize == 2 = OneP
  | otherwise = High
  where
    jokers = length $ filter (==joker) cs
    szs :: [Int]
    szs = sort $ (length <$>) <$> group $ sort $ filter (/=joker) cs
    maxSize :: Int
    maxSize = jokers + maximum szs


-- The standard Ord on lists of Ints will work for the tiebreaker
instance Ord Hand1 where
  compare :: Hand1 -> Hand1 -> Ordering
  compare = comparing (\h@(H1 cs) -> (rank1 h, cs)) 


instance Ord Hand2 where
  compare :: Hand2 -> Hand2 -> Ordering
  compare = comparing (\h@(H2 cs) -> (rank2 h, cs)) 


day7 :: IO ()
day7 = do
  ls <- getLines 7
  let g1 = parseHand <$> ls

  -- Now just sort on the parsed hands will put them in the right order
  putStrLn $ "Day7: part1: " ++ show (sum $ (\((_,s), r) -> s*r) <$> zip (sort g1) [1..])
  putStrLn $ "Day7: part2: " ++ show (sum $ (\((_,s), r) -> s*r) <$> zip (sort $ first convert <$> g1) [1..])

  return ()




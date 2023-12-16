module Day15(day15) where

import Utils (ord, getLines, splitOn)
import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.IntMap qualified as M
import Data.Foldable (Foldable(toList))


type Label = String
type Box = Seq (Label, Int)
type Boxes = M.IntMap Box


parse :: String -> [String]
parse = splitOn ','


parse2 :: String -> (Label, Label -> Boxes -> Boxes)
parse2 s
  | last s == '-' = (init s, sub)
  | otherwise = (init $ init s, add (read [last s]))


hash :: Label -> Int
hash = foldl go 0
  where
    go :: Int -> Char -> Int
    go n c = ((n + ord c) * 17) `rem` 256


sub :: Label -> Boxes -> Boxes
sub label = M.adjust go boxix
  where
    boxix = hash label
    go :: Seq (String, Int) -> Seq (String, Int)
    go = S.filter ((/=label) . fst)


add :: Int -> Label -> Boxes -> Boxes
add focus label = M.adjust go boxix
  where
    boxix = hash label
    go :: Seq (Label, Int) -> Seq (Label, Int)
    go box = case S.findIndexL ((==label). fst) box of
              Just lensix -> S.update lensix (label, focus) box
              Nothing -> box S.|> (label, focus)


score :: Boxes-> Int
score boxes = sum $ M.mapWithKey (\boxix b -> (boxix+1) * scoreBox b) boxes


scoreBox :: Box -> Int
scoreBox box = sum $ (\(lensix, (_, focus)) -> lensix * focus) <$> zip [1..] (toList box)


day15 :: IO ()
day15 = do
  ls <- getLines 15
  let g1 = parse $ ls!!0
      g2 :: [(Label, Label -> Boxes -> Boxes)]
      g2 = parse2 <$> g1
      startMap = M.fromList $ (, S.Empty)<$> [0..255]

  putStrLn $ "Day15: part1: " ++ show (sum $ hash <$> g1)
  putStrLn $ "Day15: part2: " ++ show (score $ foldl (\m (l, fn) -> fn l m) startMap g2)

  return ()

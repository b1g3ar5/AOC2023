module Day4(day4) where

import Utils (getLines)
import Data.Map (Map)
import Data.Map qualified as M
import Data.List (intersect)


type Card = ([Int], [Int])
type Cards2 = Map Int (Int, Card)


nwinners :: Int
nwinners = 10

parseCard :: String -> (Int, Card)
parseCard s = (read . init $ ws!!1, (read <$> take nwinners (drop 2 ws),  read <$> drop (nwinners + 3) ws))
  where
    ws = words s


-- This works because 2^0 `div` 2 == 0
score1 :: Card -> Int
score1 c = 2 ^ score2 c `div` 2


score2 :: Card -> Int
score2 (ws, ns) = length (ws `intersect` ns)


play :: Int -> Cards2 -> Cards2
play ix cs 
  | ix `M.notMember` cs = cs
  | otherwise = play (ix+1) $ foldl (flip (M.adjust (\(n,c)-> (n + fst (cs M.! ix),c)))) cs [(ix+1)..ix + score2 (snd $ cs M.! ix)]


day4 :: IO ()
day4 = do
  ls <- getLines 4
  let cs = M.fromList $ parseCard <$> ls

  putStrLn $ "Day4: part1: " ++ show (sum $ score1 <$> cs)
  putStrLn $ "Day4: part2: " ++ show (sum $ fst <$> play 1 ((1,) <$> cs))

  return ()

module Day8(day8) where

import Utils (sort, getLines)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M


type NodeData = String
type T = Map NodeData (NodeData, NodeData)


parse :: [String] -> (String, T)
parse ls = (head ls, M.fromList ns)
  where
    ns = parseNode <$> drop 2 ls


parseNode :: String -> (NodeData, (NodeData, NodeData))
parseNode s = (ws!!0, (init $ tail $ ws!!2, take 3 $ ws!!3))
  where
    ws = words s


run :: Int -> (String -> Bool) -> String -> T -> NodeData -> Int
run n' finish route mp = go n' 0 0
  where
    len = length route
    go :: Int -> Int -> Int -> NodeData -> Int
    go n steps ix pos
      | n == 0 = steps-1 
      | finish pos = go (n-1) (steps+1) nextix $ snd nxt
      | route!!ix == 'R' = go n (steps+1) nextix $ snd nxt
      | otherwise = go n (steps+1) nextix $ fst nxt
      where
        nxt = mp M.! pos
        nextix = (ix + 1) `mod` len


{-

I did LCM without checking - but it worked.

After the fact I chekcked that it takes the same time to get from the Z to the Z
as it does to get from the A to the Z, ie.

run 2 ((=='Z') . last) route tree "**Z" == run 1 ((=='Z') . last) route tree "**A"

-}

day8 :: IO ()
day8 = do
  ls <- getLines 8
  --let ls = test2
  let (route, tree) = parse ls
      as :: [NodeData]
      as = filter ((== 'A') . last) $ M.keys tree
      zs = filter ((== 'Z') . last) $ M.keys tree
      stepsA = run 1 ((=='Z') . last) route tree <$> as
      stepsZ = run 2 ((=='Z') . last) route tree <$> zs

  putStrLn $ "Day8: part1: " ++ show (run 1 (=="ZZZ") route tree "AAA")
  putStrLn $ "Day8: Is LCM valid: " ++ show (sort stepsA == sort stepsZ)
  putStrLn $ "Day8: part2: " ++ show (foldl1 lcm stepsA)

  return ()


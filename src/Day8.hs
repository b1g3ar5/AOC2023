module Day8(day8) where

import Utils ( swap, sort, getLines )
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


run :: (String -> Bool) -> String -> T -> NodeData -> Int
run finish route mp = go 0 0
  where
    len = length route
    go :: Int -> Int -> NodeData -> Int
    go steps ix pos
      | finish pos = steps
      | route!!ix == 'R' = go (steps+1) nextix $ snd nxt
      | otherwise = go (steps+1) nextix $ fst nxt
      where
        nxt = mp M.! pos
        nextix = (ix + 1) `mod` len


{-

I did LCM without checking - but it worked.
The justification is 

  1. The Z nodes are the A nodes with the sub nodes swapped over
  2. The number of steps to get to the z nodes are all a multiple of the route length
  3. The number of steps to get to the nodes are all ODD

So when we get to the Z nodes it is just the same as being at the A nodes

-}

day8 :: IO ()
day8 = do
  ls <- getLines 8
  --let ls = test2
  let (route, tree) = parse ls
      as :: [NodeData]
      as = filter ((== 'A') . last) $ M.keys tree
      --zs = filter ((== 'Z') . last) (M.keys tree)
      --at = sort $ (tree M.!) <$> as
      --zt = sort $ swap . (tree M.!) <$> zs
      steps = run ((=='Z') . last) route tree <$> as

  putStrLn $ "Day8: part1: " ++ show (run (=="ZZZ") route tree "AAA")
  --putStrLn $ "Day8: part1: " ++ show (run ((=='Z') . last) route tree "AAA")
  --putStrLn $ "Day8: part1: " ++ show (run ((=='Z') . last) route tree "VSD")
  --putStrLn $ "Day8: part1: " ++ show (run ((=='Z') . last) route tree "TFQ")
  --putStrLn $ "Day8: part2: " ++ show (product ((`div` length route) <$> steps) * length route)
  --putStrLn $ "Day8: part2: " ++ show (steps)
  putStrLn $ "Day8: part2: " ++ show (foldl1 lcm steps)

  return ()


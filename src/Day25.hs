module Day25(day25) where

import Utils ( sortOn, getLines, splitOn )
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Numeric.LinearAlgebra


type Part = String
type Graph = M.Map Part (S.Set Part)

parse :: String -> [(Part, S.Set Part)]
parse s = (ps!!0, S.fromList rs) : ((, S.singleton $ ps!!0) <$> rs)
  where
    ps = splitOn ':' s
    rs = words $ tail $ ps!!1


degreeMatrix :: Graph -> Matrix Double
degreeMatrix g = diag . fromList $ ds
  where
    ds = (\k -> fromIntegral $ length $ g M.! k) <$> M.keys g


adjacencyMatrix :: Graph -> Matrix Double
adjacencyMatrix g = (n><n) xss
  where
    ks = M.keys g
    n = length ks
    xss = [ if j `S.member` (g M.! k) then 1 else 0 | k <-ks, j <-ks]


laplacian :: Graph  -> Matrix Double
laplacian g = degreeMatrix g - adjacencyMatrix g


split :: Graph -> (Vector Double, Matrix Double)
split g = (l,v)
  where
    lap = laplacian g
    (l, v) = eigSH $ sym lap


day25 :: IO ()
day25 = do
  ss <- getLines 25
  --let ss = test
  let gs = parse <$> ss
      gr = foldl (\m (p,ps) -> M.insertWith S.union p ps m) M.empty $ concat gs

      (l,v) = split gr
      -- find the index of the second smallest eigen value
      ix = fst $ sortOn snd (zip [0..] $ toList l) !! 1
      n = length $ M.keys gr
      n1 = length $ filter (< 0) $ toList (tr v ! ix)

  putStrLn $ "Day25: " ++ show ((n-n1)*n1) -- 527790

  return ()


test = ["jqt: rhn xhk nvd"
  , "rsh: frs pzl lsr"
  , "xhk: hfx"
  , "cmg: qnr nvd lhk bvb"
  , "rhn: xhk bvb hfx"
  , "bvb: xhk hfx"
  , "pzl: lsr hfx nvd"
  , "qnr: nvd"
  , "ntq: jqt hfx bvb xhk"
  , "nvd: lhk"
  , "lsr: lhk"
  , "rzs: qnr cmg lsr rsh"
  , "frs: qnr lhk lsr"]
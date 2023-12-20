module Day20(day20) where

import Utils (getLines)
import Data.Map (Map)
import Data.Map qualified as M
import Data.List.Split (splitOn)


type Name = String
type Modules = Map Name Module
data Module = Flipflop Bool [Name] | Conjunction (Map Name PulseType) [Name] | Broadcast [Name] deriving (Eq, Show)
data PulseType = Low | High deriving (Eq, Show)
type Pulse = (Name, Name, PulseType)


-- To check cunjunctions...
allHigh :: Map Name PulseType -> Bool
allHigh = all (==High) . M.elems


-- Bad programming...
isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _ = False

flip' :: Name -> Modules -> Modules
flip' = M.adjust (\(Flipflop b ns) -> Flipflop (not b) ns)


parse :: [String] -> Modules
parse ls = postProcess $ M.fromList $ go <$> ls
  where
    go s
      | head (ws!!0) == '%' = (tail (ws!!0), Flipflop False ns)
      | head (ws!!0) == '&' = (tail (ws!!0), Conjunction M.empty ns)
      | ws!!0 == "broadcaster" = (ws!!0, Broadcast ns)
      | otherwise = error $ "Error in parse of module: " ++ s
      where
        ws = words s
        ns = splitOn "," $ concat $ drop 2 ws


postProcess :: Modules -> Modules
postProcess mp = foldl (\acc (name, c@(Conjunction _ ns)) -> M.insert name (Conjunction (M.fromList $ (,Low) <$> getSenders name) ns) acc ) mp cs
  where
    cs = filter (isConjunction . snd) $ M.toList mp
    getSenders :: Name -> [Name]
    getSenders c = fst <$> filter (sendsTo c) (M.toList mp)
    sendsTo :: Name -> (Name, Module) -> Bool
    sendsTo c (n,e) = case e of
                        Flipflop _ ns -> c `elem` ns
                        Conjunction _ ns -> c `elem` ns
                        Broadcast ns -> c `elem` ns


-- Runs until the pipeline has all gone and counts the high and low pulses
run1 :: (Int, Int) -> [Pulse] -> Modules -> ((Int, Int), Modules)
run1 (nlo, nhi) [] ms = ((nlo, nhi), ms)
run1 (nlo, nhi)(p@(_,_,typ):pipeline) ms = run1 (nlo + (if typ==Low then 1 else 0), nhi + (if typ==High then 1 else 0)) (pipeline ++ ps) newMs
  where
    (newMs, ps) = runPulse p ms
    

-- Runs one pulse and returs the new pulses generated
runPulse :: Pulse -> Modules -> (Modules, [Pulse])
runPulse (from, to, typ) ms
  | to `M.notMember` ms = (ms, [])
  | otherwise = 
      case toModule of
        Broadcast ns -> (ms, (to, ,Low) <$> ns)
        Flipflop False ns -> if typ == High then (ms, []) 
                                            else (flip' to ms, (to, ,High) <$> ns)
        Flipflop True ns -> if typ == High then (ms, []) 
                                          else (flip' to ms, (to, ,Low) <$> ns)
        Conjunction mp ns -> if allHigh newmp then (M.insert to (Conjunction newmp ns) ms, (to,,Low) <$> ns) 
                                              else (M.insert to (Conjunction newmp ns) ms, (to,,High) <$> ns) 
          where
            newmp = M.insert from typ mp
  where
    toModule = ms M.! to


-- Presses the button 1000 times and counts the high and low pulses
count :: Int -> (Int, Int) -> Pulse -> Modules -> (Int, (Int, Int))
count n hl p ms
  | n==1000 = (n, (newl, newh))
  | otherwise = count (n+1) (newl, newh) p newms
  where
    ((newl, newh), newms) = run1 hl [p] ms


-- kz sends to rx (our target)
-- kz is a conjunction so to send a LOW all inputs must have sent a HIGH
-- kz has 4 inputs sj, bg, ls, qq - they are all conjunctions
-- we can try to work out the cycle length for each to send a HIGH to kz


-- Presses the button until a pulse satisfies the predicate
runUntil :: Int -> (Pulse -> Bool) -> Pulse -> Modules -> Int
runUntil n f startPulse modules
  | b = n
  | otherwise = runUntil (n+1) f startPulse newModules
  where
    (b, newModules) = run2 f [startPulse] modules


-- Runs until the predicate is satisfied or the pipeline runs out
run2 :: (Pulse -> Bool) -> [Pulse] -> Modules -> (Bool, Modules)
run2 _ [] modules = (False, modules)
run2 f (pulse:pipeline) modules
  | f pulse = (True, modules)
  | otherwise = run2 f (pipeline ++ newPulses) newModules
  where
    (newModules, newPulses) = runPulse pulse modules


day20 :: IO ()
day20 = do
  ss <- getLines 20
  let g = parse ss
      (n, (l,h)) = count 1 (0,0) ("button", "broadcaster", Low) g
      names = ["qq", "ls", "bg", "sj"]
      ms = (\name -> runUntil 1 (\(from, to, typ) -> to=="kz" && from==name && typ ==High) ("button", "broadcaster", Low) g) <$> names
  putStrLn $ "Day20: part1: " ++ show ((1000 `div` n)*(1000 `div` n)*l*h) -- 944750144
  putStrLn $ "Day20: part1: " ++ show (foldl1 lcm ms) -- 222718819437131

  return ()


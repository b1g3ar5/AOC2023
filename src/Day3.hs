module Day3(day3) where

import Utils 
import Data.Set (Set, intersection, unions, fromList)
import Data.Set qualified as S


type Sym = [(Coord, Char)]
type Numbers = [(Int, Set Coord)]


-- Are any of the coords of the first set next to the second set
nextTo :: Set Coord -> Set Coord -> Bool
cs `nextTo` ds = not $ S.null $ cs `intersection` unions (S.map (fromList . neighbours8) ds)


isSymbol :: Char -> Bool
isSymbol c  = c /= '.' && not (isDigit c)


parseNumbers :: Int -> String -> [(Int, Set Coord)]
parseNumbers y s = go [] (0, S.empty) $  zip ((,y) <$>[0..]) s
  where
    -- We carry a brought forward number so that we can accumulate numbers over the line
    go :: [(Int, Set Coord)] -> (Int, Set Coord) -> [(Coord, Char)] -> [(Int, Set Coord)]
    go acc bf [] = acc ++ [bf]
    go acc bf@(i, cds) ((cd, ch):cs)
      | isDigit ch = go acc (if i==0 then read [ch] else i*10 + read [ch], cd `S.insert` cds) cs
      | otherwise = go (acc ++ [bf]) (0, S.empty) cs


parseGrid :: [String] -> (Sym, Numbers)
parseGrid css = (sym, ns)
  where
    sym :: [(Coord, Char)]
    sym = concatMap (\(y, cs) -> foldl (\acc (x, c) -> if isSymbol c then acc ++ [((x,y), c)]  else acc) [] $ zip [0..] cs) $ zip [0..] css
    ns = concatMap (uncurry parseNumbers) (zip [0..] css)


gear :: Numbers -> (Coord, Char) -> Maybe Int
gear nums (symbolCell, symbol)
  | symbol /= '*' = Nothing
  | length close == 2 = Just $ product $ fst <$> close
  | otherwise = Nothing
  where
    close = filter (\(_,cs) -> any (`nextTo8` symbolCell) cs) nums


day3 :: IO ()
day3 = do
  ls <- getLines 3
  let (sym, ns) = parseGrid ls

  putStrLn $ "Day3: part1: " ++ show (sum $ fst <$> filter (\(_, cs) -> cs `nextTo` fromList (fst <$> sym)) ns)
  putStrLn $ "Day3: part2: " ++ show (sum $ mapMaybe (gear ns) sym )

  return ()


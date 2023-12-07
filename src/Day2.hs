module Day2(day2) where

import Utils ( ReadP, string, getLines, pInt, parseS )
import Text.ParserCombinators.ReadP ( (+++), char, eof, sepBy1 )
import Control.Monad ( void )
import TotalMap qualified as T

-- Using ReadP today so that as I try to remember how it works..

data Cube = Blue | Green | Red deriving (Show, Eq, Ord)
type Grab = T.TMap Cube Int -- The TMap has a default element for missing keys
type Game = (Int, [Grab])


pRed, pBlue, pGreen, pCube :: ReadP Cube
pRed = string "red" >> return Red
pBlue = string "blue" >> return Blue
pGreen = string "green" >> return Green
pCube = pRed +++ pBlue +++ pGreen


pItem :: ReadP (Cube, Int)
pItem = do
  n <- pInt
  _ <- char ' '
  c <- pCube
  return (c,n)


pGrab :: ReadP Grab
pGrab = do
  is <- sepBy1 pItem (string ", ")
  _ <- void (char ';') +++ eof -- a grab finishes with a ; or the end of the input
  return $ T.fromList 0 is


pGame :: ReadP Game
pGame = do
  _ <- string "Game"
  _ <- char ' '
  ix <- pInt
  _ <- string ": "
  gs <- sepBy1 pGrab (string " ")
  _ <- eof
  return (ix, gs)


parseGame :: String -> Game
parseGame = fst . head . parseS pGame


checkGame :: Game -> Bool
checkGame (_, gs) = all (\g -> (g T.! Red) <= 12 && (g T.! Green) <= 13 && (g T.! Blue) <= 14) gs


minGame :: Game -> Int
minGame (_, gbs) = findMax Red * findMax Blue * findMax Green
  where
    findMax :: Cube -> Int
    findMax c = maximum $ (T.! c) <$> gbs


day2 :: IO ()
day2 = do
  ls <- getLines 2
  let gs = parseGame <$> ls

  putStrLn $ "Day2: part1: " ++ show (sum $ fst <$> filter checkGame gs) 
  putStrLn $ "Day2: part2: " ++ show (sum $ minGame <$> gs) 

  return ()


module Main (main) where

import Lib (libMain)
import Utils

main :: IO ()
main = timeIt libMain


makeFiles :: IO ()
makeFiles = mapM_ makeFile [2..25]

makeFile :: Int -> IO ()
makeFile n = do 
  writeFile ("./src/Day" ++ show n ++ ".hs") (proforma n)


proforma :: Int -> String
proforma n = unlines [
  "module Day" ++ show n ++ "(day" ++ show n  ++ ") where"
  , ""
  , "import Utils"
  , ""
  , ""
  , "parse :: String -> Int"
  , "parse = read"
  , ""
  , ""
  , "day" ++ show n ++ " :: IO ()"
  , "day" ++ show n ++ " = do"
  , "  ss <- getLines " ++ show n
  , "  let g = parse <$> ss"
  , ""
  , "  putStrLn $ \"Day" ++ show n ++ ": part1: \" ++ show g"
  , "  putStrLn $ \"Day" ++ show n ++ ": part2: \" ++ show \"\""
  , ""
  , "  return ()"
  ]  

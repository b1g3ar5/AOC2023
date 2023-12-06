
module Day1(day1) where

import Utils
import Data.Trie qualified as T
--import qualified Data.ByteString.Lazy.Char8 as C

import Data.ByteString qualified as B
--import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as TS
import Data.Text.Encoding qualified as TS
--import Data.Text.IO qualified as T
--import Data.Text.Lazy qualified as TL
--import Data.Text.Lazy.Encoding qualified as TL
--import Data.Text.Lazy.IO qualified as TL


notDigit :: ReadP String
notDigit = many (satisfy (\c -> ord c < ord '0' || ord c > ord '9'))


parse1 :: ReadP Int
parse1 = do
  _ <- notDigit
  ds <- sepBy1 digit notDigit
  return $ read [head ds] * 10 + read [last ds]


dStrs :: [String]
dStrs = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


numbers, reverseNumbers :: T.Trie Int
numbers = T.fromList $ zip (TS.encodeUtf8 . TS.pack <$> dStrs) [1..] ++ zip (TS.encodeUtf8 . TS.pack . show <$> [1..9]) [1..9]
reverseNumbers = T.fromList $ zip (TS.encodeUtf8 . TS.pack . reverse <$> dStrs) [1..] ++ zip (TS.encodeUtf8 . TS.pack . show <$> [1..9]) [1..9]


parse2 :: B.ByteString -> Int
parse2 is = go numbers is * 10 + go reverseNumbers (B.reverse is)
  where
    go :: T.Trie Int -> B.ByteString -> Int
    go tr s' = case tr `T.match` s' of
                  Nothing -> if B.null s' then 0 else go tr (B.drop 1 s')
                  Just (_, i, _) -> i


day1 :: IO ()
day1 = do
  ls <- getLines 1

  timeIt $ putStrLn $ "Day1: part1: " ++ show (sum $ fst . last . parseS parse1 <$> ls)
  timeIt $ putStrLn $ "Day1: part1: " ++ show (sum $ parse2 . TS.encodeUtf8 . TS.pack <$> ls)

  return ()

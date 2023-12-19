module Day14(day14) where

import Utils (getLines, fromJust)
import Data.Trie as T (Trie, insert, lookup, member, empty) -- for the memoisation
import Data.ByteString qualified as B
import Data.Bits.Utils qualified as C


tiltRt, tiltLt, tiltUp, tiltDn :: [B.ByteString] -> [B.ByteString]
tiltRt ls  = go <$> ls
  where
    go :: B.ByteString -> B.ByteString
    go s = B.intercalate (B.pack $ C.c2w8 <$> "#") $ B.sort <$> B.split (C.c2w8 '#') s
tiltDn = B.transpose . tiltRt . B.transpose
tiltLt = (B.reverse <$>). tiltRt . (B.reverse <$>)
tiltUp = reverse . tiltDn . reverse


cycle1 :: [B.ByteString] -> [B.ByteString]
cycle1 = tiltRt . tiltDn . tiltLt . tiltUp


score :: [B.ByteString] -> Int
score ls = fst $ foldl go (0,0) ls
  where
    my = length ls
    go :: (Int, Int) ->  B.ByteString -> (Int, Int)
    go (tot, n) cs = (tot + (my - n) * B.length (B.filter (== C.c2w8 'O') cs), n+1)


run :: Int -> [B.ByteString] -> Int
run  = go T.empty
  where
    go :: T.Trie Int -> Int -> [B.ByteString] -> Int
    go seen n ls
      | n==0 = s
      | rep ls `T.member` seen && n < cyc = go newSeen (n-1) $ cycle1 ls
      | rep ls `T.member` seen = go newSeen (n-reps*cyc-1) $ cycle1 ls
      | otherwise = go newSeen (n-1) $ cycle1 ls
      where
        rep = B.concat -- the representation of the grid is just the lines concatenated
        cyc = fromJust (rep ls `T.lookup` seen ) - n
        reps = n `div` cyc
        s = score ls
        newSeen = T.insert (rep ls) n seen


day14 :: IO ()
day14 = do
  ls <- getLines 14

  let bs = B.pack . (C.c2w8 <$>) <$> ls
  putStrLn $ "Day14: part1: " ++ show (score $ tiltUp bs)
  putStrLn $ "Day14: part2: " ++ show (run 1000000000 bs)

  return ()




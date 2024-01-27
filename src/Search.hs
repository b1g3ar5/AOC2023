{-# LANGUAGE RecordWildCards     #-}

module Search where

import           Control.Arrow       ((>>>))
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array.IArray   as IA
import           Data.Array.ST
import           Data.Array.Unboxed  (UArray)
import           Data.Array.Unsafe   (unsafeFreeze)
import           Data.Sequence       (Seq (..), ViewL (..), (|>))
import qualified Data.Sequence       as Seq
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         ((!), HashMap)
import qualified Data.HashMap.Strict         as HM


import Enumeration ( Enumeration(..) )

------------------------------------------------------------
-- Faster bfs copied from Brent Yorgey
-- https://byorgey.wordpress.com/2021/11/17/competitive-programming-in-haskell-bfs-part-4-implementation-via-stuarray/
------------------------------------------------------------

infixl 0 >$>
(>$>) :: a -> (a -> b) -> b
(>$>) = flip ($)
{-# INLINE (>$>) #-}

exhaustM :: Monad m => (a -> m (Maybe a)) -> a -> m a
exhaustM f = go
  where
    go a = do
      ma <- f a
      maybe (return a) go ma

------------------------------------------------------------
-- BFS
------------------------------------------------------------

data Result v = Result { getLevel :: v -> Maybe Int, getParent :: v -> Maybe v }

type V = Int
data State s = BS { level :: STUArray s V Int, parent :: STUArray s V V, queue :: Seq V }


initBFSState :: Int -> [Int] -> ST s (State s)
initBFSState n vs = do
  l <- newArray (0,n-1) (-1)
  p <- newArray (0,n-1) (-1)

  forM_ vs $ \v -> writeArray l v 0
  return $ BS l p (Seq.fromList vs)


bfs :: forall v. Enumeration v -> [v] -> (v -> [v]) -> (v -> Bool) -> Result v
bfs Enumeration{..} vs next goal
  = toResult $ bfs' card (map locate vs) (map locate . next . select) (goal . select)
  where
    toResult :: (forall s. ST s (State s)) -> Result v
    toResult m = runST $ do
      st <- m
      (level' :: UArray V Int) <- unsafeFreeze (level st)
      (parent' :: UArray V V) <- unsafeFreeze (parent st)
      return $
        Result
          ((\l -> guard (l /= -1) >> Just l) . (level' IA.!) . locate)
          ((\p -> guard (p /= -1) >> Just (select p)) . (parent' IA.!) . locate)

visited :: State s -> V -> ST s Bool
visited BS{..} v = (/= -1) <$> readArray level v
{-# INLINE visited #-}

bfs' :: Int -> [V] -> (V -> [V]) -> (V -> Bool) -> ST s (State s)
bfs' n vs next goal = do
  st <- initBFSState n vs
  exhaustM bfsStep st
  where
    bfsStep :: State s -> ST s (Maybe (State s))
    bfsStep st@BS{..} = case Seq.viewl queue of
      EmptyL -> return Nothing
      v :< q'
        | goal v -> return Nothing
        | otherwise -> do 
                     nxt <- filterM (fmap not . visited st) (next v)
                     nextState <- foldM (update v) (st{queue=q'}) nxt
                     return $ Just nextState

    update :: V -> State s -> V -> ST s (State s)
    update p b@BS{..} v = do
      lp <- readArray level p
      writeArray level v (lp + 1)
      writeArray parent v p
      return $ b{queue = queue |> v}



--------------------------------------------------------

data State' v = BS' { level' :: HashMap v Int, parent' :: HashMap v v, queue' :: Seq v }

exhaust :: (a -> Maybe a) -> a -> a
exhaust f = go
  where
    go a = maybe a go (f a)


initBFSState' :: (Eq v, Hashable v) => [v] -> State' v
initBFSState' vs = BS' (HM.fromList (map (,0) vs)) HM.empty (Seq.fromList vs)


--bfsSlow :: forall v. (Eq v, Hashable v) => [v] -> (v -> [v]) -> (v -> Bool) -> Result v
bfsSlow :: (Eq v, Hashable v) => [v] -> (v -> [v]) -> (v -> Bool) -> Result v
bfsSlow pipeline next finish = toResult $ exhaust bfsStep (initBFSState' pipeline)
  where
    toResult BS'{..} = Result (`HM.lookup` level') (`HM.lookup` parent')
    bfsStep st@BS'{..} = case Seq.viewl queue' of
      EmptyL -> Nothing
      v :< q'
        | finish v    -> Nothing
        | otherwise -> Just $ foldl (upd v) (st{queue'=q'}) (filter (not . (`HM.member` level')) (next v))
        -- | otherwise -> v >$> next >>> filter (not . (`HM.member` level')) >>>
                       --foldl (upd v) (st{queue'=q'}) >>> Just
    upd p BS'{..} v = BS'
      (HM.insert v l level')
      (HM.insert v p parent')
      (queue' |> v)
      where
        l = level'!p + 1
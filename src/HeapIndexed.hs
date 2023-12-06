module HeapIndexed where

import Data.Foldable (Foldable(foldl')) 


data Heap k a = EmptyHeap | Heap k [a] (Heap k a) (Heap k a) deriving (Eq, Ord, Functor, Foldable, Show)


isEmpty :: Heap k a -> Bool
isEmpty EmptyHeap = True
isEmpty _ = False


size :: Heap k a -> Int
size EmptyHeap = 0
size (Heap _ _ l r) = 1 + size l + size r


singleton :: k -> a -> Heap k a
singleton i x = Heap i [x] EmptyHeap EmptyHeap


union :: (Ord k, Ord a) => Heap k a -> Heap k a -> Heap k a
EmptyHeap `union` t2 = t2
t1 `union` EmptyHeap = t1
t1@(Heap k1 xs1 l1 r1) `union` t2@(Heap k2 xs2 l2 r2)
  | k1 == k2 = Heap k1 (xs1 ++ xs2) (l2 `union` r2 `union` r1) l1
  | k1 > k2 = Heap k1 xs1 (t2 `union` r1) l1
  | otherwise = Heap k2 xs2 (t1 `union` r2) l2


insert :: (Ord k, Ord a) => Heap k a -> k -> a -> Heap k a
insert heap i xs = singleton i xs `union` heap


fromList ::  (Ord k, Ord a) => [(k,a)] -> Heap k a
fromList = foldl' (\acc (i,x) -> insert acc i x) EmptyHeap


toList :: Heap k a -> [(k,[a])]
toList t = inorder t []
  where
    inorder :: Heap k a -> [(k,[a])] -> [(k,[a])]
    inorder EmptyHeap xs = xs
    inorder (Heap i x l r) xs = inorder l ((i,x) : inorder r xs)


extractMin ::  (Ord k, Ord a) => Heap k a -> Maybe ((k,a), Heap k a)
extractMin EmptyHeap = Nothing
extractMin (Heap _ [] _ _) = error "This should not be possible in extractMin"
extractMin (Heap i [x] l r) = Just ((i,x), l `union` r)
extractMin (Heap i (x:xs) l r) = Just ((i,x), Heap i xs l r)


deleteMin ::  (Ord k, Ord a) => Heap k a -> Maybe (Heap k a )
deleteMin h = snd <$> extractMin h



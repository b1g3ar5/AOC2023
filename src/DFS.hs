module DFS where

import Data.Array.IArray -- (Array, indices, bounds, array)
import Data.Array.ST
import Data.Tree
import Control.Monad.ST
import Data.List ( (\\) )

-- This implementation needs a list of all the states
-- ie the bounds of the indices of the array

type Vertex = Int
type Edge = (Vertex, Vertex)
type Table a = Array Vertex a
type Bounds = (Vertex, Vertex)
type Graph = Table [Vertex]
type Set s = STArray s Vertex Bool


vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [(v,w) | v <- vertices g, w <- g ! v]


mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (v, f v $ t ! v) | v <- indices t]


outDegree :: Graph -> Table Int
outDegree = mapT (\_ ws -> length ws)


inDegree :: Graph -> Table Int
inDegree = outDegree . transposeG


buildG :: Bounds -> [Edge] -> Graph
buildG = accumArray (flip (:)) []


transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)


reverseE :: Graph -> [Edge]
reverseE g = [(w,v) | (v,w) <- edges g]


preorder :: Tree a -> [a]
preorder (Node a ks) = a : concatMap preorder ks


preOrd :: Graph -> [Vertex]
preOrd g = concatMap preorder $ dff g


postorder :: Tree a -> [a]
postorder (Node a ks) = concatMap postorder ks ++ [a]


postOrd :: Graph -> [Vertex]
postOrd g = concatMap postorder $ dff g


topSort :: Graph -> [Vertex]
topSort = reverse . postOrd


components :: Graph -> Forest Vertex
components = dff . undirected 


scc' :: Graph -> Forest Vertex
scc' g = dfs (transposeG g) $ reverse $ postOrd g


scc :: Graph -> Forest Vertex
scc g = dfs g $ reverse $ postOrd $ transposeG g


undirected :: Graph -> Graph
undirected g = buildG (bounds g) $ edges g ++ reverseE g


tabulate :: Bounds -> [Vertex] ->Table Int
tabulate bs vs = array bs (zip vs [1..])


preArr :: Bounds -> Forest Vertex ->Table Int
preArr bs ts = tabulate bs $ concatMap preorder ts


generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v $ generate g <$> g ! v


dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (bounds g) (generate g <$> vs)


dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)


mkEmpty :: Bounds -> ST s (Set s) 
mkEmpty bs = newArray bs False

contains :: Set s -> Vertex -> ST s Bool
contains = readArray

include :: Set s -> Vertex -> ST s ()
include s v = writeArray s v True


prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bs ts = runST $ mkEmpty bs >>= \s -> chop s ts


chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop _  [] = return []
chop s (Node v ks : us) = contains s v >>= \visited -> 
                            if visited then 
                              chop s us else
                              --include s v >>= \_ ->
                              include s v >>
                              chop s ks   >>= \as -> 
                              chop s us   >>= \bs ->
                              return $ Node v as : bs


chop' :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop' _  [] = return []
chop' s (Node v ks : us) = do 
  visited <- contains s v
  if visited 
    then chop' s us 
    else do
      include s v
      as <- chop' s ks
      bs <- chop' s us
      return $ Node v as : bs


reachable :: Graph -> Vertex -> [Vertex]
reachable g v = concatMap preorder $ dfs g [v]


path :: Graph -> Vertex -> Vertex -> Bool
path g from to = to `elem` reachable g from


-- Classification of graph edges

tree :: Bounds -> Forest Vertex -> Graph
tree bs ts = buildG bs $ concatMap flat ts
  where
    flat (Node v ks) = [(v, w) | (Node w _) <- ks ] ++ concatMap flat ks

back :: Graph -> Table Int -> Graph
back g t = mapT select g
  where
    select v ws =     [w | w <- ws, t!v < t!w]

cross :: Graph -> Table Int -> Table Int -> Graph
cross g pre post = mapT select g
  where
    select v ws = [w | w <- ws, post!v > post!w, pre!v > pre!w]


forward :: Graph -> Graph -> Table Int -> Graph
forward g h pre = mapT select g
  where
    select v ws = [w | w <- ws, pre!v < pre!w] \\ h ! v


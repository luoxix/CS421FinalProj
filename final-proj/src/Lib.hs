--- Getting Started
--- ===============

module Lib where
import Data.Array
import Control.Monad.ST
import Data.Array.ST

--- Implement Graph
type Table a = Array Char a
type Graph = Table [Char]

vertices :: Graph -> [Char]
vertices = indices

type Edge = (Char, Char)
edges :: Graph -> [Edge]
edges g = [(v,w) | v <- vertices g, w <- g!v]

------ methods to manipulate graph / retrieve graph info
mapT :: (Char -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v)) | v <- indices t]

type Bounds = (Char, Char)
outdegree :: Graph -> Table Int 
outdegree g = mapT numEdges g
    where numEdges v ws = length ws

buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [ (w,v) | (v,w) <- edges g ]

indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)

--- Implement DFS
data Tree a = Node a (Forest a)
              deriving (Eq, Show)
type Forest a = [Tree a]

dfs :: Graph -> [Char] -> Forest Char 
dfs g vs = prune (bounds g) (map (generate g) vs)

dff :: Graph -> Forest Char
dff g = dfs g (vertices g)

------ Generating
generate :: Graph -> Char -> Tree Char
generate g v = Node v (map (generate g) (g!v))

------ Pruning
type Set s = STArray s Char Bool

mkEmpty :: Bounds -> ST s (Set s)
mkEmpty bnds = newArray bnds False

contains :: Set s -> Char -> ST s Bool
contains m v = readArray m v

include :: Set s -> Char -> ST s ()
include m v = writeArray m v True

prune :: Bounds -> Forest Char -> Forest Char
prune bnds ts 
    = runST (mkEmpty bnds >>= \m -> 
             chop m ts)

chop :: Set s -> Forest Char -> ST s (Forest Char)
chop m [] = return []
chop m (Node v ts : us)
    = contains m v >>= \visited ->
        if visited then
            chop m us
        else
            include m v     >>= \_     ->
            chop m ts       >>= \as    ->
            chop m us       >>= \bs    ->
            return ((Node v as) : bs)

--- DFS Applications
------ Numbering
preorder :: Tree a -> [a]
preorder (Node a ts) = [a] ++ preorderF ts

preorderF :: Forest a -> [a]
preorderF ts = concat (map preorder ts)

preOrd :: Graph -> [Char]
preOrd g = preorderF (dff g)

tabulate :: Bounds -> [Char] -> Table Int
tabulate bnds vs = array bnds (zip vs [1..])

preArr :: Bounds -> Forest Char -> Table Int
preArr bnds ts = tabulate bnds (preorderF ts)

------ Topological sorting
postorder :: Tree a -> [a]
postorder (Node a ts) = postorderF ts ++ [a]

postorderF :: Forest a -> [a]
postorderF ts = concat (map postorder ts)
postOrd g = postorderF (dff g)

topSort :: Graph -> [Char]
topSort g = reverse (postOrd g)

------ Connected components
components :: Graph -> Forest Char
components g = dff (undirected g)

undirected :: Graph -> Graph
undirected g = buildG (bounds g) (edges g ++ reverseE g)

------ Strong connected components
scc :: Graph -> Forest Char
scc g = dfs (transposeG g) (reverse (postOrd g))

scc' :: Graph -> Forest Char
scc' g = dfs g (reverse (postOrd (transposeG g)))

------ Finding reachable vertices
reachable :: Graph -> Char -> [Char]
reachable g v = preorderF (dfs g [v])

path :: Graph -> Char -> Char -> Bool
path g v w = w `elem` (reachable g v)
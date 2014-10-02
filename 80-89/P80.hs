import Data.Monoid
import Data.List (union, unionBy)
import Control.Applicative ( (<$>), (<*>) )

data Graph a = Graph [a] [(a, a)]
                deriving (Show, Eq)
newtype Adjacency a = Adjacency [(a, [a])]
                deriving (Show, Eq)

newtype Friend a = Friend [(a, a)]
                deriving (Show, Eq)


instance Monoid (Adjacency a) where
  mempty = Adjacency []
  Adjacency xs `mappend` Adjacency ys = Adjacency (xs ++ ys)

instance (Eq a) => Monoid (Graph a) where
  mempty = Graph [] []
  (Graph n1 e1) `mappend` (Graph n2 e2) = Graph (union n1 n2) (unionBy (\(m, n) (j, k) -> (m == j && n == k) || (m == k && n == j)) e1 e2)

exampleGraph :: Graph Char
exampleGraph = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

graphToAdj :: (Eq a, Ord a) => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adjacency []
graphToAdj (Graph (x:xs) edges) = Adjacency [(x, adjEdges)] `mappend` (graphToAdj $ Graph xs edges)
          where adjEdges = collectEdges edges
                collectEdges [] = []
                collectEdges ((a,b):ys)
                      | a == x = b:collectEdges ys
                      | b == x = a:collectEdges ys
                      | otherwise = collectEdges ys

adjToGraph :: (Eq a, Ord a) => Adjacency a -> Graph a
adjToGraph (Adjacency ls) = foldr helperFunction (Graph [] []) ls
    where helperFunction (node, edges) g = Graph [node] (edgesToNode node edges) `mappend` g
          edgesToNode n xs = map (\x -> (n, x)) $ filter (> n) xs

graphToFriend :: (Eq a, Ord a) => Graph a -> Friend a
graphToFriend (Graph nodes edges) = Friend (concat $ map helper nodes)
    where helper n = let result = filter (\(a,b) -> a == n || b == n) edges
                     in if null result
                        then [(n,n)]
                        else filter ((==n) . fst) result

friendToGraph :: (Eq a, Ord a) => Friend a -> Graph a
friendToGraph (Friend edges) = foldr helper (Graph [] []) edges
          where helper t@(a,b) g = (if a == b
                                    then Graph [a] []
                                    else Graph [a, b] [t]) `mappend` g


friendToAdj :: (Eq a, Ord a) => Friend a -> Adjacency a
friendToAdj = graphToAdj . friendToGraph

adjToFriend :: (Eq a, Ord a) => Adjacency a -> Friend a
adjToFriend = graphToFriend . adjToGraph

-- p81
-- example :
-- findPaths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[1,2,3,4],[1,3,4]]
-- findPaths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []

type Acyclic a = [(a, a)]

findPaths :: (Eq a) => a -> a -> Acyclic a -> [[a]]
findPaths _ _ [] = []
findPaths start end acy
     | start == end = [[end]]
     | otherwise = let cands = filter ((== start) . fst) acy
                       remains = filter ((/= start) . fst) acy 
                       helper (x, y) = (:) <$> [x] <*> findPaths y end remains
                   in concat $ map helper cands

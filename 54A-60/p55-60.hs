import Data.List (findIndex)
import Data.Maybe (fromJust)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf a = Branch a Empty Empty

cbalTree ::(Integral n) => n -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n 
    | odd n = [ Branch 'x' x x | x <- cbalTree ((n-1)`div` 2)]
    | otherwise =  [ Branch 'x' x y | x <- cbalTree ((n-1)`div` 2), y <- cbalTree (n `div` 2)] ++
                    [ Branch 'x' y x | x <- cbalTree ((n-1)`div` 2), y <- cbalTree (n `div` 2)]


-- p56
mirror' :: Tree a -> Tree a -> Bool
mirror' Empty Empty = True
mirror' _ Empty = False
mirror' Empty _ = False
mirror' (Branch _ l1 r1) (Branch _ l2 r2) = mirror' l1 r2 && mirror' r1 l2

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror' l r

-- p 57
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of 
                    LT -> Branch y (add x l) r
                    GT -> Branch y l (add x r)
                    EQ -> t

construct :: (Ord a) => [a] -> Tree a
construct = foldl (flip add) Empty

--p58
symCbalTrees :: (Integral a) => a -> [Tree Char]
symCbalTrees 0 = [Empty]
symCbalTrees n 
        | odd n = [Branch 'x' l (mirrorTree l)| l <- cbalTree ((n-1) `div` 2) ]
        | otherwise = [] 

mirrorTree Empty = Empty
mirrorTree (Branch x l r) = Branch x (mirrorTree r) (mirrorTree l)

--p59

--height balanced binary tree , will take the height as parameter, return all the
--trees
{-hbalTree :: a -> Int -> [Tree a]
hbalTree c 0 = [Empty]
hbalTree c 1 = [(Branch c Empty Empty)]
hbalTree c n = [ Branch c l r| l <- hbalTree c (n-1) , r <- hbalTree c (n-2)] ++
                    [ Branch c l r| l <- hbalTree c (n-2), r <- hbalTree c (n-1)] ++
                    [ Branch c l r| l <- hbalTree c (n-1) , r <- hbalTree c (n-1)]
-}
hbalTree :: a -> Int -> [Tree a]
hbalTree x h = trees !! h
  where trees = [Empty] : [Branch x Empty Empty] :
                zipWith combine (tail trees) trees
        combine ts shortts = [Branch x l r |
                (ls, rs) <- [(shortts, ts), (ts, ts), (ts, shortts)],
                l <- ls, r <- rs]

--p60 
miniNode :: Int -> Int
miniNode h = nodesList !! h 
                where nodesList = 0 : 1 :
                                zipWith ((+).(+1)) (tail nodesList) nodesList


maxHeight :: Int -> Int
maxHeight n = (fromJust $ findIndex (>n) nodesList) -1                          
                where nodesList = 0 : 1 :
                                zipWith ((+).(+1)) (tail nodesList) nodesList
minHeight n = ceiling $ logBase 2 (fromIntegral n+1)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes c n = [x | height <- [minHeight n .. maxHeight n], x <-hbalTree c height, countNodes x == n]
                    where countNodes Empty = 0
                          countNodes (Branch _ l r) = 1+countNodes l + countNodes r 
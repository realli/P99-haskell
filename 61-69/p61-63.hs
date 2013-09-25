import Data.List(group)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf a = Branch a Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

--p61
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch a Empty Empty) = 1
countLeaves (Branch a l r) = countLeaves l + countLeaves r

--p61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a l r) = leaves l ++ leaves r

--p62
internals :: Tree a -> [a]
internals tr = internals' tr []
                where internals' Empty xs = xs
                      internals' (Branch a Empty Empty) xs = xs
                      internals' (Branch a l r) xs = (a:) $ internals' l $ internals' r xs
--p62B
atLevel :: Tree a -> Int -> [a]
atLevel tr level = atLevel' tr level []
                    where atLevel' Empty          _     xs = xs
                          atLevel' (Branch a l r) 1     xs = a:xs
                          atLevel' (Branch a l r) level xs = atLevel' l (level - 1) $ atLevel' r (level - 1) xs
--p63
completeBinaryTree :: (Integral a) => a -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch 'x' Empty Empty
completeBinaryTree n = Branch 'x' (completeBinaryTree leftNum) (completeBinaryTree rightNum)
                        where ratio = floor $ logBase 2 (fromIntegral n+1) - 1
                              standard = 2^ratio - 1
                              over = n-1 - 2*standard
                              leftNum = if over > 2^ratio then standard + 2^ratio else standard + over
                              rightNum = n-1 - leftNum
completeBinaryTree' :: Int -> Tree Char
completeBinaryTree' n = construct 1
                        where construct x
                                | x > n = Empty
                                | otherwise = Branch 'x' (construct (2*x)) (construct (2*x +1))


isCompleteBinaryTree' :: Tree a -> Bool
isCompleteBinaryTree' Empty = True
isCompleteBinaryTree' (Branch x l r) = isCompleteBinaryTree l && isCompleteBinaryTree r


filled :: Tree a -> [[Bool]]                                                                                                                                   
filled Empty = repeat [False]                                                                                                                                  
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)                                                                                            
 
isCompleteBinaryTree :: Tree a -> Bool                                                                                                                      
isCompleteBinaryTree Empty = True                                                                                                                           
isCompleteBinaryTree t = and $ last_proper : zipWith (==) lengths powers                                                                                    
  where levels      = takeWhile or $ filled t                                                                                                                  
        -- The upper levels of the tree should be filled.                                                                                                      
        -- Every level has twice the number of nodes as the one above it,                                                                                      
        -- so [1,2,4,8,16,...]                                                                                                                                 
        lengths     = map (length . filter id) $ init levels                                                                                                   
        powers      = iterate (2*) 1                                                                                                                           
        -- The last level should contain a number of filled spots,                                                                                             
        -- and (maybe) some empty spots, but no filled spots after that!                                                                                       
        last_filled = map head $ group $ last levels                                                                                                           
        last_proper = head last_filled && (length last_filled) < 3


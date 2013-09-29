data Tree a = Node a [Tree a] deriving (Show, Eq)

--some example

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
--p70C
nnodes :: Tree a -> Int
nnodes (Node a ls) = 1 + sum (map nnodes ls)
--p70 
treeToString :: Tree Char -> [Char] 
treeToString (Node c ls) = c:(concat (map treeToString ls)++"^")
stringToTree :: String -> Tree Char
stringToTree (c:cs) = Node c (fst $ go cs)
        where go ('^':xs) = ([], xs)
              go (x:xs) = ((Node x nodes):(fst $ go rest ), snd $ go rest)
                    where nodes = fst res
                          rest = snd res 
                          res = go xs

--p71
ipl :: Tree a -> Int
ipl  = go 0  
        where go :: Int -> Tree a -> Int
              go n (Node _ []) = n 
              go n (Node _ ls) = n + sum (map (go (n+1)) ls)

--p72 
bottom_up :: Tree Char -> String 
bottom_up (Node c ls) = (concatMap bottom_up ls) ++ [c]

bottom_up' :: Tree Char -> String
bottom_up' t = go t []
                where go (Node c ls) xs = foldr go (c:xs) ls

--p73
lisp :: Tree Char -> String 
lisp (Node c []) = [c]
lisp (Node c ls) = '(':c:' ':(unwords $ map lisp ls)++")"


data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf a = Branch a Empty Empty

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )



treeToString ::  Tree Char -> String 
treeToString Empty = ""
treeToString (Branch c l r) 
        | l == Empty && r == Empty = [c] 
        | otherwise =  [c] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++")" 


stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree (x:y:xs) = Branch x left right
                        where (left, right) = splitToTrees $ init xs
                              splitToTrees str = (\(a,b) -> (stringToTree a, stringToTree b)) $ splitTreeString str 0
                            
splitTreeString :: String -> Int -> (String, String)
splitTreeString [] _ = ("","")
splitTreeString (x:xs) n 
        | n == 0 && x == ',' = ("", xs)
        | x == '(' =  addX $ splitTreeString xs (n+1)
        | x == ')' = addX $ splitTreeString xs (n-1)  
        | otherwise = addX $ splitTreeString xs n
            where addX (a,b) = (x:a,b)


--p68
treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch c l r) = c: treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch c l r) = treeToInorder l ++ c: treeToInorder r

preToTree :: String -> Tree Char
preToTree "" = Empty
preToTree (c:cs) = Branch c Empty (preToTree cs)

preIntTree :: String -> String ->Tree Char
preIntTree [] _ = Empty
preIntTree [x] _ = Branch x Empty Empty
preIntTree (x:xs) ys = Branch x (preIntTree prel inl) (preIntTree prer inr)
                    where (inl, _:inr) = break (== x) ys
                          (prel, prer) = splitAt (length inl) xs 

--p69
tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch c l r) = c:tree2ds l ++ tree2ds r

ds2tree :: String -> Tree Char
ds2tree [] = undefined
ds2tree "." = Empty
ds2tree (x:xs) = Branch x (ds2tree leftStr) (ds2tree rightStr)
                where (leftStr, rightStr) = splitHelper xs 0
                      splitHelper l 1 = ([],l)
                      splitHelper l@(y:ys) n 
                                    | y == '.' = addY $ splitHelper ys (n+1)
                                    | otherwise = addY $ splitHelper ys (n-1)
                                        where addY (a,b) = (y:a,b)

ds2tree' []      = (Empty,"")                                                                                                                                   
ds2tree' ('.':xs) = (Empty,xs)                                                                                                                                  
ds2tree' (x:xs)  = (Branch x left right, rest2)                                                                                                                 
  where (left,rest) = ds2tree' xs                                                                                                                               
        (right,rest2) = ds2tree' rest
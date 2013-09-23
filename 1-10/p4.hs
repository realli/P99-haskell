myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' = foldl (\acc ele-> acc + 1) 0 

myLength'' = foldr (\ele acc -> acc+1) 0 

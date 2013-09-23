myLast [x] = x
myLast (_:xs) = myLast xs

myLast' ls = foldl1 (\x y-> y)ls

myLast'' ls = ls !! (length ls - 1)

myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

myButLast' = last.init 

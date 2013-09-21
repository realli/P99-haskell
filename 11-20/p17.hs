split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split l 0 = ([], l)
split ls n 
      | length ls < n = (ls, [])
      | otherwise = helper ([],ls) n
      				where helper tup 0 = tup
      				      helper (a, (x:xs)) n = helper (a++[x], xs) (n-1)



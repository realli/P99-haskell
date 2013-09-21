rotate :: [a] -> Int -> [a]
rotate ls@(x:xs) n
      | n==0 = ls
      | n > 0 = rotate (xs++[x]) (n-1)
      | n < 0 = rotate ((last ls):(init ls)) (n+1)
repli :: [a] -> Int ->[a]
repli ls n = concat $ map (helper n) ls
				where helper n x = replicate n x


repli' :: [a] -> Int ->[a]
repli' ls n = concat $ foldr helper [] ls
				where helper x l = replicate n x : l
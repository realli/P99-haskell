insertAt :: Char -> String -> Int -> String
insertAt c ls n = take (n-1) ls ++ (c:drop (n-1) ls)

insertAt' :: Char -> String -> Int -> String
insertAt' c ls n = foldl helper [] $ zip [1..] ls
					where helper l (a,b) 
								| a == n = l ++ [c,b]
								| otherwise = l ++ [b]
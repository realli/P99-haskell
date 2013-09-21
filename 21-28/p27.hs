import Data.List (tails,delete)

groupp :: (Eq a) => [Int] -> [a] -> [[[a]]]
groupp [] ls = [[]]
groupp (i:ic) ls = do
	(start, rest) <- combination i ls
	end <- groupp ic rest
	return (start:end)



combination' :: Int -> [a] -> [[a]]
combination' 0 _ = [[]]
combination' k xs = [ y:zs | y:ys <- tails xs, 
					   zs <- combination' (k-1) ys]

combination :: (Eq a) => Int -> [a] ->[([a], [a])]
combination k xs = map (foldr helper ([],xs) ) $ combination' k xs
						where helper ele (a,b) = (ele:a, delete ele b) 
	
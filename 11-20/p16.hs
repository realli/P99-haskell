dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery ls n = (if length first == n then init first else first) ++ 
					dropEvery second n
						where (first,second) = splitAt n ls

dropEvery' :: [a] -> Int -> [a]
dropEvery' ls n = [ i | (i,c) <- zip ls $ cycle [1..n], c/=n]
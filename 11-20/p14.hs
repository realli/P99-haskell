dupli :: [a] -> [a]
dupli = concatMap helper 
		where helper x = [x,x]

dupli' :: [a] -> [a]
dupli' = foldr helper []
		where helper x result = x:x:result

dupli'' [] = []
dupli'' (x:xs) = x:x:dupli''xs 
removeAt :: Int -> [a] -> [a]
removeAt n ls = [x | (x,i) <- zip ls [1..], i /= n]

removeAt' :: Int -> [a] -> [a]
removeAt' n =  map snd . filter ((n/=) .fst) . zip [1..] 
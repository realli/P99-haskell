import Control.Monad (guard)
type Comb = []
combinations :: (Ord a) => Int -> [a] ->[Comb a]
combinations 1 ls = map (\x -> [x]) ls
combinations n ls = combinations (n-1) ls >>= one_comb ls

one_comb ::(Ord a) => [a] -> Comb a -> [Comb a]
one_comb ls comb = do 
    (x:xs) <- map (:comb) ls
    guard (x `isTheMini` xs)
    return (x:xs)
        where a `isTheMini` b = all (>a) b 
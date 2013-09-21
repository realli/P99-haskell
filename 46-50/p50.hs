import Data.List
import Data.Ord (comparing)

--Htree can have Int(frequence),but none is ok
data HTree a = Leaf a| Branch (HTree a) (HTree a) deriving (Show)

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,String)]
huffman  = sortBy (comparing fst) .encode . htree . map (\(a,w) -> (w, Leaf a)). sortBy (comparing snd)
    where htree [(_, t)] = t
          htree ((w1,t1):(w2,t2):xs) = htree $ insertBy (comparing fst) ((w1+w2), Branch t1 t2) xs
          encode (Leaf x) = [(x,"")]
          encode (Branch l r) = [ (x,'0':c) | (x,c) <- encode l] ++ [ (x,'1':c) | (x,c)<-encode r]
import System.Random
import Data.List
import Data.Function (on)
ran_permu :: StdGen -> [a] -> [a]
ran_permu gen ls = map fst .sortBy (compare `on` snd) . zip ls.take (length ls).nub.randomRs (1, length ls) $ gen

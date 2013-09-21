import Data.List (nub)
import System.Random
diff_select :: Int -> Int -> [Int]
diff_select n range = take n . nub $ randomRs (1, range) $ mkStdGen 1991
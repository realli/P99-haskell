import Data.List (intercalate)
import Control.Monad (replicateM)
not' :: Bool -> Bool
not' True  = False
not' False = True

and' True True = True
and' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' a b= not' $ and' a b

or' False False = False
or' _ _ = True

nor' a b  = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

equ' a b = not'$ xor' a b

impl' a b = (not' a) `or'` b

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` 

table' :: (Bool -> Bool -> Bool) ->[[Bool]]
table' f = [ [x, y, f x y]| x <- [True,False], y <-[True, False]]

table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ (putStrLn.intercalate " ". map show) $ table' f

table'' :: Int -> ([Bool] -> Bool) ->[[Bool]]
table'' n f = [x ++ [(f x)]| x <- replicateM n [True,False]]

table1 :: Int -> ([Bool] -> Bool) -> IO()
table1 n f = mapM_ (putStrLn.intercalate " ".map show) $ table'' n f

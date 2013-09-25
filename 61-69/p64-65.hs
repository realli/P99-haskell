data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf a = Branch a Empty Empty



--p64
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout :: Tree a -> Tree (a,(Int, Int))
layout tr = fst $ layout' tr (1,1)
                where layout' Empty (x,_) = (Empty, x)
                      layout' (Branch c l r) (x,y) = ( (Branch (c, (xLeft, y)) left right) ,  xRight)
                                                    where (left, xLeft) = layout' l (x, y+1)
                                                          (right, xRight) = (layout' r (xLeft + 1, y+1))


--p65
{- a little confuse althought worked
layout65 :: Tree a -> Tree (a,(Int, Int))
layout65 tr = fst $ layout' tr (1,1)
                where layout' Empty (x, y) = (Empty, x)
                      layout' (Branch c l r) (x,y) = ( (Branch (c, (xLeft , y)) left right) , xLeft + 2^(ymax - y))
                                                    where (left, xLeft) = layout' l (x, y+1)
                                                          right = put r (xLeft + 2^(ymax - y-1), y+1)
                                                                where put Empty (p,q) = Empty
                                                                      put (Branch c l r) (p,q) = Branch (c,(p, q)) (put l (p-2^(ymax-q-1), q+1)) (put r (p+2^(ymax-q-1), q+1))
                      ymax = cal tr
                                where cal Empty = 0
                                      cal (Branch c l r) = 1 + max (cal l) (cal r)

-}
layout65 ::Tree a -> Tree (a, (Int, Int))
layout65 tr = layout' (x1, 1) sep1 tr
                where dep = depth tr
                      leftDep = leftDepth tr
                      sep1 = 2 ^ ( dep -2 )
                      x1 = 2 ^ (dep - 1) - 2^(dep - leftDep) + 1
                      layout' (x, y) sep Empty = Empty
                      layout' (x, y) sep (Branch c l r) = Branch (c, (x, y)) 
                                                                (layout' (x-sep, y+1) (sep `div` 2) l)
                                                                (layout' (x+sep, y+1) (sep `div` 2) r)

depth :: Tree a -> Int
depth Empty = 0
depth (Branch c l r) = 1 + max (depth l) (depth r)

leftDepth :: Tree a -> Int
leftDepth Empty = 0
leftDepth (Branch c l r) = 1 + leftDepth l   

--p66
layout66 :: Tree a -> Tree (a , (Int, Int))
layout66 tr = layout' (1, 1) tr
            where layout' _ Empty = Empty
                  layout' (x,y) (Branch c l r) = Branch (c, (newX, y)) left newRight
                            where shiftN = merge left right 0
                                  left =  layout' (x, y+1) l
                                  right = layout' (x, y+1) r
                                  newRight = shiftTree right shiftN
                                  newX = (shiftN + topX left) `div` 2
                  topX Empty = 0
                  topX (Branch (c, (x',_)) _ _) = x'
                  merge Empty _  n  = if n == 0 then 2 else n
                  merge (Branch (_, (xl,_)) l1 r1)    Empty n = if n == 0 then xl+2 else n 
                  merge (Branch (_, (xl,_)) l1 r1) (Branch (_, (xr,_)) l2 r2) n = merge r1 l2 (n + if xr - xl < 2 then 2 + xl - xr else 0)
                  shiftTree Empty n = Empty
                  shiftTree (Branch (c, (x',y')) l r) n = Branch (c, (x'+n, y')) (shiftTree l n) (shiftTree r n)                                                    
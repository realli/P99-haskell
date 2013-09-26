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
layout66 tr = decodeTuple $ layout' (1, 1) tr
            where layout' (x,y) Empty  = (Empty, [], [])
                  layout' (x,y) (Branch c l r) = ( (Branch (c, (nodeX, y)) leftTree newRightTree), nodeX:lSideLeft, nodeX:sideRight )
                        where (leftTree, lSideLeft, lSideRight) = layout' (x, y+1) l
                              (rightTree, rSideLeft, rSideRight) = layout' (x, y+1) r
                              shiftNum = calShift lSideRight rSideLeft
                              newRightTree = shiftTree shiftNum rightTree
                              sideRight = shiftList shiftNum rSideRight
                              nodeX = calNodeX lSideLeft sideRight
                  calNodeX [] _ = 1
                  calNodeX lt [] = head lt + 1
                  calNodeX lt rt = ((head lt) + (head rt)) `div` 2

                  calShift [] [] = 0
                  calShift [] rt = if head rt == 1 then 1 else 0
                  calShift _ [] = 0
                  calShift lsl lsr = maximum $ zipWith ((-).(+2)) lsl lsr

                  shiftTree _ Empty = Empty
                  shiftTree num (Branch (c,(x',y')) l r) = Branch (c, (x'+num ,y')) (shiftTree num l) (shiftTree num r)
                  shiftList num ls = map (+num) ls 

                  decodeTuple (a,_,_) = a


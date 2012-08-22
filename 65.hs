import MyTree

type Pos = (Int, Int)

sampleTree = head $ compTree' 2

conDisTree :: Tree a -> Tree (Pos,a)
conDisTree Empty = Empty
conDisTree (Branch a t0 t1) = helper 0 0 (Branch a t0 t1)
    where helper x y Empty = Empty
          helper x y (Branch a' tl tr) = 
            let l = lefty  tl
                r = l
                pos = (l,r)
            in  Branch ((x,y),a') (helper (x - l) (y + 1) tl) (helper (x + r) (y + 1) tr)

lefty Empty = 0
lefty (Branch _ tll tlr)   = leftHelper tll + 1

leftHelper Empty = 0
leftHelper (Branch _ l r)  = leftHelper l + rightHelper r + 1

rightHelper Empty = 0
rightHelper (Branch _ l r) = leftHelper l + rightHelper r + 1 

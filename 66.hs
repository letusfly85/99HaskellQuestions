import MyTree

type Pos = (Int, Int)

conDisTree :: Tree a -> Tree (Pos,a)
conDisTree Empty = Empty
conDisTree (Branch a t0 t1) = helper 0 0 (Branch a t0 t1)
    where helper x y Empty = Empty
          helper x y (Branch a' tl tr) = 
            let l = lefty  tl
                r = righty tr
                pos = (l,r)
            in  case (l,r) of
              (1,1) -> Branch ((x,y),a') (helper (x - l) (y + 1) tl) (helper (x + r) (y + 1) tr)
              (1,_) -> Branch ((x,y),a') (helper (x - l) (y + 1) tl) (helper (x + r -1) (y + 1) tr)
              (_,1) -> Branch ((x,y),a') (helper (x - l + 1) (y + 1) tl) (helper (x + r) (y + 1) tr)
              (_,_) -> Branch ((x,y),a') (helper (x - l) (y + 1) tl) (helper (x + r) (y + 1) tr)

lefty Empty = 0
lefty (Branch _ tll tlr)   = leftHelper tll + 1

righty Empty = 0
righty (Branch _ trl trr)  = rightHelper trr + 1

leftHelper Empty = 0
leftHelper (Branch _ l r)  = leftHelper l + rightHelper r + 1

rightHelper Empty = 0
rightHelper (Branch _ l r) = leftHelper l + rightHelper r + 1 

sampleTree = head $ compTree' 2

myHelper :: Tree a -> Pos
myHelper Empty = (0,0)
myHelper (Branch _ Empty Empty) = (1,0)
myHelper (Branch _ tl tr) = let lPos = myHelper tl
                                rPos = myHelper tr
                            in  (1 + fst lPos, 1 + snd rPos)

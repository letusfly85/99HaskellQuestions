import MyTree

type Pos = (Int, Int)      

vTree :: Tree a -> Tree (a, Pos)
vTree t = fst (layoutAux (0,0) t)

s0 = Branch 'x' Empty Empty
s1 = Branch 'x' s0 s0
s2 = Branch 'x' (Branch 'x' s0 Empty) s1

layoutAux :: Pos -> Tree a -> (Tree (a, Pos), Pos)
layoutAux (x, y) (Branch node tl tr) = (Branch (node,(x',y')) tl' tr', (x'', y''))
            where (tl',(x',  y' ))   = layoutAux (x       , (y + 1)) tl
                  (tr',(x'', y''))   = layoutAux ((x' + 1), (y + 1)) tr


hTree :: Tree a -> Tree Pos
hTree tree = helper 0 0 tree
    where helper x y Empty = Empty
          helper x y (Branch _ tl tr) = 
            let l = lefty  tl
                r = righty tr
            in  Branch (x,y) (helper (x - l) (y + 1) tl) (helper (x + r) (y + 1) tr)

lefty Empty = 0
lefty (Branch _ tll tlr) = leftHelper tll + 1

righty Empty = 0
righty (Branch _ trl trr) = rightHelper trr + 1

leftHelper Empty = 0
leftHelper (Branch _ l r) = leftHelper l + rightHelper r + 1

rightHelper Empty = 0
rightHelper (Branch _ l r) = leftHelper l + rightHelper r + 1 

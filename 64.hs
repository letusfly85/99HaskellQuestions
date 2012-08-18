import MyTree

type Pos = (Int, Int)      

vTree :: Tree a -> Tree (a, Pos)
vTree t = fst (layoutAux 1 1 t)
    where layoutAux x y (Branch node tl tr) = (Branch (node,(x',y)) tl' tr', x'')
            where (tl',x')  = layoutAux x        (y + 1) tl
                  (tr',x'') = layoutAux (x' + 1) (y + 1) tr

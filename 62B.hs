import MyTree

hBalTree :: a -> Int -> [Tree a]
hBalTree node 0 = [Empty]
hBalTree node 1 = [Branch node Empty Empty]
hBalTree node h = [Branch node l r |
    (hl, hr) <- [(h-1, h-1),(h-2, h-1),(h-1, h-2)]
    , l <- hBalTree node hl, r <- hBalTree node hr]

collectNodes :: Tree a -> Int
collectNodes Empty = 0
collectNodes (Branch node Empty Empty) = 1
collectNodes t = let (Branch node tl tr) = t
                 in  collectNodes tl + collectNodes tr

heightTree :: Tree a -> Int
heightTree Empty = 0
heightTree (Branch node Empty Empty) = 1
heightTree t = let (Branch node tl tr) = t
                   tlh = heightTree tl
                   trh = heightTree tr
               in  case tlh `compare` trh of
                      LT -> tlh
                      GT -> trh
                      EQ -> tlh

atLevelNodes :: Tree a -> Int -> Int
atLevelNodes Empty _ = 0
atLevelNodes t 1 = 1
atLevelNodes t h
    | (heightTree t) < h = error "argument error: "
    | (heightTree t) > h = atLevelNodes' t h 0
    where atLevelNodes' t h acc =  let (Branch node  tl tr) = t
                                       count = (atLevelNodes tl (h-1)) + (atLevelNodes tr (h-1))
                                   in if acc == h then count
                                      else atLevelNodes' t h (acc+1)


collectAllNodes :: Int -> Int -> [Int]
collectAllNodes h l = map (flip atLevelNodes l) (hBalTree 'x' h)

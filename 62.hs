import MyTree

internals :: Tree a -> [a]
internals Empty = []
internals (Branch node Empty Empty) = []
internals (Branch node _     Empty) = [node]
internals (Branch node Empty _    ) = [node]
internals t = let (Branch node tl tr) = t
              in case (tr , tr) of
              (Empty, tr   ) -> node:internals tr
              (tl   , Empty) -> node:internals tl
              (tl   ,    tr) -> internals tl ++ internals tr

hBalTree :: a -> Int -> [Tree a]
hBalTree node 0 = [Empty]
hBalTree node 1 = [Branch node Empty Empty]
hBalTree node h = [Branch node l r |
    (hl, hr) <- [(h-1, h-1),(h-2, h-1),(h-1, h-2)]
    , l <- hBalTree node hl, r <- hBalTree node hr]



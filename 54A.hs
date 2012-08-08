data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                    (Branch 'e' Empty Empty))
            (Branch 'c' Empty
                (Branch 'f' (Branch 'g' Empty Empty)
                    Empty))

leaf x = Branch x Empty Empty

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                    (leaf 'e'))
            (Branch 'c' Empty
                    (Branch 'f' (leaf 'g')
                        Empty))

tree2 = Branch 'a' Empty Empty

tree3 = Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
            (Branch 2 Empty Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show,Eq)

compBaiTree :: Int -> a -> Tree a
compBaiTree 0 a = Empty
compBaiTree 1 a = Branch a Empty Empty
compBaiTree n a = let (res, bit) = ((n-1) `div` 2, (n-1) `rem` 2)
                  in case bit of
                     0 -> Branch a (compBaiTree res a)     (compBaiTree res a)
                     1 -> Branch a (compBaiTree (res+1) a) (compBaiTree res a)

completeBinaryTree :: Int -> a -> Tree a
completeBinaryTree n a = generate_tree 1 a
    where generate_tree x a
            | x > n     = Empty
            | otherwise = Branch a (generate_tree (2*x) a) (generate_tree (2*x+1) a)


isSame ::  (Eq a) => Int -> a -> Bool
isSame n a  = if (compBaiTree n a) == (completeBinaryTree n a) then True else False

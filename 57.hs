import MyTree

addT :: a -> Tree a -> Tree a -> Tree a
addT node x y = Branch node x y

addL :: a -> Tree a -> Tree a
addL node x = addT node x Empty

addLF :: [a] -> Tree a
addLF [] = Empty
addLF (x:xs) = addL x (addLF xs)

addR :: a -> Tree a -> Tree a
addR node x = addT node Empty x

addRF :: [a] -> Tree a
addRF [] = Empty
addRF (x:xs) = addR x (addRF xs)

constructT :: [a] -> Tree a
constructT []       = Empty
constructT (x:list) = let (lList,rList) = (evenList list, oddList list)
                      in addT x (addLF lList) (addRF rList)

oddList list  = [ list !! idx | idx <- take (length list `div` 2) $ filter odd  [0..]]
evenList list = [ list !! idx | idx <- take ((length list + 1) `div` 2) $ filter even [0..]]

addTree :: (Ord a) => a -> Tree a -> Tree a
addTree x Empty = Branch x Empty Empty
addTree x t@(Branch n l r) = case compare x n of
            LT -> Branch n (addTree x l) r
            GT -> Branch x l (addTree n r)
            EQ -> t

construct xs = foldl (flip addTree) Empty xs

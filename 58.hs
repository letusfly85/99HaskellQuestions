import MyTree

symCbalTree :: Int -> [Tree Int]
symCbalTree x = filter isSymmetric' $ compTree' x

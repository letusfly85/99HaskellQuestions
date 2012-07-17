import MyCombination

type ThreeInt = (Int, Int, Int)

groupThreeList :: (Eq a) => [a] -> ThreeInt -> [([a],[a],[a])]
groupThreeList []   (s0,s1,s2) = []
groupThreeList list (s0,s1,s2) = let g0 = chooseList list s0
                                     g1 = [mapTwin g0' $ chooseList (getRest list g0') s1 | g0' <- g0 ] 
                                     g2 = [mapTriple (g11,g12) $ chooseList (getRest list (g11 ++ g12)) s1 | (g11,g12) <- concat g1 ] 
				 in  concat g2

mapTwin :: (Eq a) => [a] -> [[a]] -> [([a],[a])]
mapTwin x [] = [(x,[])]
mapTwin x list = [(x,l) | l <- list]

mapTriple :: (Eq a) => ([a],[a]) -> [[a]] -> [([a],[a],[a])]
mapTriple (a1,a2) []   = [(a1,a2,[])]
mapTriple (a1,a2) list = [(a1,a2,a3) | a3 <- list]

chooseList :: (Eq a) => [a] -> Int -> [[a]]
chooseList _  0 = []
chooseList [] s = []
chooseList list s = nubList $ nestMap (list !!) $ [0..length list -1] `combination` s

nestMap :: (a -> b) -> [[a]] -> [[b]]
nestMap f []     = []
nestMap f (x:xs) = map f x : nestMap f xs

getRest :: (Eq a) => [a] -> [a] -> [a]
getRest [] _ = []
getRest list xs = foldr getRest' [] list
	where getRest' l acc = if l `elem` xs then acc
	                       else l:acc

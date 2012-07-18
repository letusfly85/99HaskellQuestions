import Data.List (sortBy,group,groupBy)
import Data.Ord  (compare)

-- a) List Length Sort
llSort :: (Ord a) => [[a]] -> [[a]]
llSort nList = sortBy lengthCompare nList
    where lengthCompare x y = if length x > length y then GT
                              else LT

-- b) List Frequency Sort
lfSort :: (Ord a) => [a] -> [a]
lfSort []     = []
lfSort (l:ls) = lfSort [n | n <- ls, n /= l] ++ [l] ++ lfSort [n | n <- ls, n == l] 

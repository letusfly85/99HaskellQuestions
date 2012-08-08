import Data.List (sortBy)

isListEq :: (Eq a, Ord a) => [a] -> [a] -> Bool
isListEq x y = if (x `isListEq'` y) && (y `isListEq'` x) then True
               else False

isListEq' :: (Eq a, Ord a) => [a] -> [a] -> Bool
isListEq' []  _    = True
isListEq' _   []   = False
isListEq' (x:xs) y = if (x `elem` y) then isListEq' xs y
                     else False

nubList :: (Eq a, Ord a) => [[a]] -> [[a]]
nubList list = nubList' (map (sortBy comp) list)
    where comp x y = x `compare` y
nubList' [] = []
nubList' (x:xs) = if any (isListEq x) xs then nubList xs
                  else x : nubList xs

combination :: [Int] -> Int -> [[Int]]
combination n 0 = [n]
combination n k = nubList $ map (take k) $ concat [map ((n !! i):) (nubList $ combination (removeAt (i + 1) n) (k - 1)) | i <- [0..length n -1]]

removeAt :: Int -> [Int] -> [Int]
removeAt _    []   = []
removeAt pos  list = take (pos -1) list ++ drop pos list 

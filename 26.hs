isListEq :: (Eq a) => [a] -> [a] -> Bool
isListEq x y = if (x `isListEq'` y) && (y `isListEq'` x) then True
               else False

isListEq' :: (Eq a) => [a] -> [a] -> Bool
isListEq' []  _    = True
isListEq' _   []   = False
isListEq' (x:xs) y = if (x `elem` y) then isListEq' xs y
                     else False

nubList :: (Eq a) => [[a]] -> [[a]]
nubList [] = []
nubList (x:xs) = if any (isListEq x) xs then nubList xs
                 else x : nubList xs

combination :: [Int] -> Int -> [[Int]]
combination n k = nubList $ combination' n k

combination' :: [Int] -> Int -> [[Int]]
combination' n 0 = [n]
combination' n k = map (take k) $ concat [map ((n !! i):) (combination (removeAt (i + 1) n) (k - 1)) | i <- [0..length n -1]]

removeAt :: Int -> [Int] -> [Int]
removeAt _    []   = []
removeAt pos  list = take (pos -1) list ++ drop pos list 

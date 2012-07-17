dupli :: [Int] -> [Int]
dupli [] = []
dupli xs = foldr (\x acc -> replicate 2 x ++ acc) [] xs

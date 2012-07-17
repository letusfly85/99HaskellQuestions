rotate :: [a] -> Int -> [a]
rotate [] n = []
rotate xs n = if n > 0 then drop n xs ++ take n xs
                       else let n' = length xs + n
                            in  drop n' xs ++ take n' xs 

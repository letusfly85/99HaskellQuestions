insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt _ _  0 = error  "argument error: don't use Zero 0." 
insertAt target lst n = if   length lst < n then
                             error $ "argument error: use smaller Number than " ++ show (length lst)
                        else take (n -1) lst ++ [target] ++ drop (n -1) lst

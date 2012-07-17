slice :: [a] -> Int -> Int -> [a]
slice []   x y = []
slice lst  x y = if   (x > y) || (length lst < x + y -1 ) then []
                 else drop (x -1) $ take (x + y - 1) lst

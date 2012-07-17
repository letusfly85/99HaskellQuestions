removeAt :: Int -> [a] -> (a,[a])
removeAt n [] = (undefined,[undefined])
removeAt x lst = if   length lst < x then (undefined,[undefined])
                 else (lst !! (x -1), take (x-1) lst ++ drop x lst)
                      

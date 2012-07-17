repli :: String -> Int -> String
repli [] _ = []
repli str n = concatMap (replicate n) str

myrepli :: [a] -> Int -> [a]
myrepli xs n = xs >>= replicate n

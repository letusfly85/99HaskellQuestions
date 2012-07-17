split :: String -> Int -> (String,String)
split str n = let (pre,lst) = (take n str, drop n str)
              in  (pre,lst)

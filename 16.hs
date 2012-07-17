dropEvery :: String -> Int -> String
dropEvery []  _ = []
dropEvery str n = let (pre,lst)   = (take n str,drop n str)
                  in  if length pre == 1 then
                         pre !! 0 : (dropEvery lst n)
                      else
                         (init pre) ++ (dropEvery lst n)

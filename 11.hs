-- TODO: use a function 'group'

encodeModified :: String -> [(Int,Char)]
encodeModified [] = []
encodeModified str = encodeModified' [] (head str) (tail str)

encodeModified' :: [(Int, Char)] -> Char -> String -> [(Int, Char)]
encodeModified' []  c xs = encodeModified' [(1,c)] (head xs) (tail xs)
encodeModified' acc c [] = let l = last acc
                           in    if   (snd l) == c then
                                      (init acc ++ [(fst l + 1 , snd l)])
                                 else (acc ++ [(1,c)])
encodeModified' acc c xs = let l = last acc
                           in    if   (snd l) == c then
                                      encodeModified' (init acc ++ [(fst l + 1 , snd l)]) (head xs) (tail xs)
                                 else encodeModified' (acc ++ [(1,c)]) (head xs) (tail xs)

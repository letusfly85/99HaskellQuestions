-- TODO: use a function 'concatMap'

decodeModified :: [(Int,Char)] -> String
decodeModified [] = []
-- decodeModified (x:xs) = replicate (fst x) (snd x) ++ decodeModified xs
decodeModified xs = foldr (\x acc -> replicate (fst x) (snd x) ++ acc) "" xs 

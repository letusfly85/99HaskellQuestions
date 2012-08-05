import Data.List (sortBy,group)

sortString :: [Char] -> [Char]
sortString str = sortBy fnc str
    where fnc a b | a > b = GT
                  | otherwise = LT

sortByFlq :: [(Char,Int)] -> [(Char,Int)]
sortByFlq list = sortBy fnc list
    where fnc a b | snd a > snd b = LT
                  | otherwise     = GT

flqChar :: [Char] -> [(Char,Int)]
flqChar str = sortByFlq $ map (\x -> (head x, length x)) $ group $ sortString str

huffmanCode :: [Char] -> [(Char,String)]
huffmanCode str = let list = flqChar str
                  in  fnc list
    where fnc list =  zipWith (\x y -> (fst x,y)) list (huffman list)

huffman :: [a] -> [String]
huffman  [] = []
huffman  list = let pre       = init list
                    nList     = [0..length pre - 1]
                in  [flip (++)  "0" $ concat $ take i $ repeat (show 1) | i <- nList] ++ 
                    [concat $ take (length pre) (repeat (show 1))]

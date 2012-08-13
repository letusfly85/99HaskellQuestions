import MyTree

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves t = let (Branch node tl tr) = t
                in case (tl,tr) of
                (Empty,Empty) -> 1
                (Empty,tr   ) -> countLeaves tr
                (tl   ,Empty) -> countLeaves tl
                otherwise     -> countLeaves tl + countLeaves tr

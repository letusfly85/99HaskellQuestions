import MyTree

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch node left right) = leaves left ++ leaves right

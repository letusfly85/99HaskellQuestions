myList :: [a] -> a
myList [] = error "arguments error,a list is empty."
myList x = last x

import Data.Ord (compare)

myGCD :: Int -> Int -> Int
myGCD l r = case (l `compare` r) of
	GT -> myGCD (l-r) r
	LT -> myGCD l     (r-l)
	EQ -> l

coprime :: Int -> Int -> Bool
coprime l r 
    | myGCD l r == 1 = True
    | otherwise      = False

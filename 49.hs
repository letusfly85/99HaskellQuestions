func1 :: Int -> [Int] -> Int
func1 n (x:xs) = if n < 2^x then (x-1)
                else func1 n xs

func2 :: Int -> [Int] -> [Int]
func2 _ []  = []
func2 n (x:xs) = if n >= 2^x then
                    1:(func2 (n-2^x) xs)
                 else
                    0:(func2 n xs)

binary :: Int -> [Int]
binary n = let n'   = func1 n [1..]
               list = reverse [0..n']
           in  func2 n list

shiftRight :: [Int] -> [Int]
shiftRight []     = []
shiftRight (x:xs)
    | x == 1 = 0:(init (x:xs))
    | x == 0 = 1:(init (x:xs))

and'  :: Bool -> Bool -> Bool
and' a b  | a && b    = True
          | otherwise = False

xor'  :: Bool -> Bool -> Bool
xor' a b  | a `and'` b          = False
          | not a `and'`  not b = False
          | otherwise  = True

convertTF :: [Int] -> [Bool]
convertTF list = foldr f [] list
    where f x ys = if x == 1 then True:ys else False:ys

reverseTF :: [Bool] -> [Int]
reverseTF list = foldr f [] list
    where f x ys = if x == True then 1:ys else 0:ys

grayCode :: Int -> [Int]
grayCode n = let list  = binary n
                 list' = shiftRight list
             in  reverseTF $ zipWith xor' (convertTF list) (convertTF list')

table :: (Bool -> Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show (f a b c)
                          | a <- [True,False] , b <- [True,False], c <- [True,False]]

and'  :: Bool -> Bool -> Bool
and' a b  | a && b    = True
          | otherwise = False

or'   :: Bool -> Bool -> Bool
or' a b   | a = True
          | b = True
          | otherwise = False

nand' :: Bool -> Bool -> Bool
nand' a b | a `and'` b = False
          | otherwise  = True

xor'  :: Bool -> Bool -> Bool
xor' a b  | a `and'` b = False
          | a `or'`  b = True
          | otherwise  = False

nor'  :: Bool -> Bool -> Bool
nor' a b  | a `or'` b = False
          | otherwise = True

impl' :: Bool -> Bool -> Bool
impl' a b | a `and'` b = True
          | b          = False
          | otherwise  = True

equ'  :: Bool -> Bool -> Bool
equ'  a b | a `and'` b         = True
          | not a `and'` not b = True
          | otherwise          = False

infixl 7 `equ'`
infixl 4 `or'`
infixl 6 `and'`

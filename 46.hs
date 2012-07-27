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

eq'   :: Bool -> Bool -> Bool
eq'   a b | a `xor'` b = True
          | otherwise  = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
                          | a <- [True,False] , b <- [True,False]]

{-
 - answer:http://www.haskell.org/haskellwiki/99_questions/Solutions/41
 -}

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True 
isPrime n = let n' = round $ sqrt $ fromIntegral n
            in all (\x -> n `mod` x /= 0) [2..n']

primeFactors :: (Integral a) => a -> a -> [a]
primeFactors x y
    | x > y     = primeFactors' y x
    | otherwise = primeFactors' x y
    where primeFactors' m n =  filter isPrime [m..n]


goldBachList :: (Integral a) => a -> a -> [(a,a)]
goldBachList s e =  concat $ foldr (\x ys -> goldBaches x : ys ) [] $
                    filter even [s..e]

goldBaches :: (Integral a) => a -> [(a,a)]
goldBaches n = if not $ isSafeNumber n then [] else
               let m = n `div` 2
                   (pre,lst) = (primeFactors 53 m, primeFactors (m+2) n)
               in goldBach' pre lst
   where goldBach' pre lst = filter (\(p,l) -> p + l == n) $
                             [(p,l) | p <- pre, l <- lst]

isSafeNumber :: (Integral a) => a -> Bool
isSafeNumber n
    | even n = True
    | n > 106 = True
    | otherwise = False

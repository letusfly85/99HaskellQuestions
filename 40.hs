{-
 - answer:http://www.haskell.org/haskellwiki/99_questions/Solutions/40
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

goldBach :: (Integral a) => a -> (a,a)
goldBach n = if odd n then error "argument should be an even number." else
             let m = n `div` 2
                 (pre,lst) = (primeFactors 2 m, primeFactors (m+2) n)
             in goldBach' pre lst
   where goldBach' pre lst = head $ filter (\(p,l) -> p + l == n) $
                             [(p,l) | p <- pre, l <- lst]

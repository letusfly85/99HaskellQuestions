import Data.List

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

useAsPat :: [Int] -> [Int]
useAsPat [] = []
useAsPat xs@(x:t) = t

gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                | True  = k : gaps (k+2) xs

pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t

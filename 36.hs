import Data.List (sortBy, group)
import Data.Ord (compare)

isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True 
isPrime n = let n' = round $ sqrt $ fromIntegral n
            in all (\x -> n `mod` x /= 0) [2..n']

primeFactors :: (Integral a) => a -> [a]
primeFactors n = let list = filter isPrime [2..n]
                 in  foldr function [] list
    where function m ms = if n `rem` m == 0 then
                             m : primeFactors (n `div` m)
                          else
                             ms

primeFactorsMult :: (Integral a) => a -> [(a,Int)]
primeFactorsMult n = groupByTupple $ primeFactors n
    where groupByTupple []   = []
          groupByTupple list = let list' = sortBy (\x y -> x `compare` y) list
                               in  [(head x, length x) | x <- group list']


isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True 
isPrime n = let n' = round $ sqrt $ fromIntegral n
            in all (\x -> n `mod` x /= 0) [2..n']

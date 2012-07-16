import System.Random (getStdRandom, randomR)
import Control.Monad (liftM2)

randomSelect :: [a] -> Int -> IO [a]
randomSelect _  0 = return []
randomSelect xs n = do p <- getRndInt xs
                       concatList (return [xs !! p] ) (randomSelect (slice xs p) (n - 1))

concatList :: IO [a] -> IO [a] -> IO [a]
concatList x y = liftM2 (++) x y

getRndInt :: [a] -> IO Int
getRndInt xs = getStdRandom (randomR (0,length xs - 1))

slice :: [a] -> Int -> [a]
slice xs n = take (n - 1) xs ++ drop n xs

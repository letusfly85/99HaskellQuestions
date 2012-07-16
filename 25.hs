import System.Random
import Data.List (nub)

randomPermutation :: Random r => [r] -> IO [r]
randomPermutation lst = reRange lst $ getRndInt (length lst)

reRange :: [r] -> IO [Int] -> IO [r]
reRange xs list = do list' <- list
                     return [xs !! pos | pos <- list']

getRndInt :: Int -> IO [Int]
getRndInt n = do newStdGen >>= return . take n . nub. randomRs (0,n-1)

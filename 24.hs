import System.Random
import Control.Monad (replicateM)

randomSelect :: Int -> Int -> IO [Int]
randomSelect n l = randomSelect' n [1..l]

randomSelect' :: Int -> [Int] -> IO [Int]
randomSelect' 0 xs = return []
randomSelect' n xs = do pos <- replicateM n $ getRndInt xs
			return [xs !! p | p <- pos]

getRndInt :: [a] -> IO Int
getRndInt xs = getStdRandom (randomR (0,length xs - 1))


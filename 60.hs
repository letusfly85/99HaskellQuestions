import MyTree
import MyCombination

type Height = Int

maxNode :: Height -> Int
maxNode h = 2^h - 1

recCombination :: [Int] -> [[Int]]
recCombination nlist = foldr (\k ys -> (nlist `combination` k) ++ ys) [] [1..length nlist-1]

hbalTree :: Int -> [Tree Int]
hbalTree 1 = [leaf 0]
hbalTree n = let (patternList,(idxStart,idxEnd)) = allPattern n
             in  map (\x -> compTree'' $ splitRange' (snd $ getRange n) x) patternList
    where compTree'' list = if  length list /= 1 then compTree'' $ tree2TuppleList $ map getNode list
                           else getNode $ head list

allPattern :: Int -> ([[Int]],(Int,Int))
allPattern n = let (s,l) = getRange n
               in  if snd s == n then ([[1]],s)
                   else (recCombination ([[fst l..snd l] !! i | i <- [0,1..(fst l)-1]]) , s)

ans n = length $ hbalTree n

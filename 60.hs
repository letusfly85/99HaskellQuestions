import MyTree
import MyCombination
import Maybe (fromJust)
import Data.List


maxNode :: Int -> Int
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

numOfNodes :: Tree a -> Int
numOfNodes Empty = 0
numOfNodes (Branch node Empty Empty) = 1
numOfNodes tree = let (Branch node tl tr) = tree
                  in  1 + numOfNodes tr + numOfNodes tl

hBalTree :: a -> Int -> [Tree a]
hBalTree node 0 = [Empty]
hBalTree node 1 = [Branch node Empty Empty]
hBalTree node h = [Branch node l r |
    (hl, hr) <- [(h-1, h-1),(h-2, h-1),(h-1, h-2)]
    , l <- hBalTree node hl, r <- hBalTree node hr]

minNode :: Int -> Int
minNode h = minimum $ map numOfNodes $ hBalTree 'x' h

minNodeSeq = 0:1:zipWith ((+).(+1)) minNodeSeq (tail minNodeSeq)
minNodes = (minNodeSeq !!)


balTreeHeight :: Int -> [Int]
balTreeHeight 1 = [1]
balTreeHeight 2 = [2,2]
balTreeHeight n = map (+1) $ balTreeHeight (n-1) ++ balTreeHeight (n-2)

hbalTreeNode _ 0 = [Empty]
hbalTreeNode x n = concatMap toFilteredTrees [minHeight..maxHeight]
    where toFilteredTrees = filter ((n ==) . numOfNodes) . hBalTree x
          minHeight = ceiling $ logBase 2 $ fromIntegral (n+1)
          maxHeight = (fromJust $ findIndex (>n) minNodeSeq) -1

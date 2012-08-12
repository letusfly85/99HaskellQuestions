import MyTree
import MyCombination

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


{-
 - the answer on the below site
 - http://www.haskell.org/haskellwiki/99_questions/Solutions/59
 -}

type Height = Int

hBalTree :: Height -> a -> [Tree a]
hBalTree 0 node = [Empty]
hBalTree 1 node = [Branch node Empty Empty]
hBalTree h node = [Branch node l r |
    (hl, hr) <- [(h-1, h-1),(h-2, h-1),(h-1, h-2)]
    , l <- hBalTree hl node, r <- hBalTree hr node]

import MyCombination

nodeList :: [(Int,Int)]
nodeList = let list = map (2^) [0..]
           in zipWith (\i l -> (l,l + (sum $ fst $ splitAt i list))) [0..] list

getRange :: Int -> ((Int,Int),(Int,Int))
getRange n = getRange' nodeList
    where getRange' (x:y:ys) = 
            if snd x <= n && n < snd y then (x,y)
            else getRange' (y:ys)

treePattern :: Int -> ([[Int]],(Int,Int))
treePattern n = let (s,l) = getRange n
                in  if snd s == n then ([[1]],s)
                    else (combination [1..(fst l)] (n - snd s) , s)

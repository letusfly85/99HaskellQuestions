module MyTree where

import MyCombination

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

firstTree :: Tree Char
firstTree = Empty

leaf x = Branch x Empty Empty

leftConTree :: Tree a -> a -> Tree a
leftConTree tree node = Branch node tree Empty

rightConTree :: Tree a -> a -> Tree a
rightConTree tree node = Branch node Empty tree

bothConTree :: a -> Tree a -> Tree a -> Tree a
bothConTree node t0 t1 = Branch node t0 t1

instance Monad Tree where
    return    a  = Empty
    Empty >>= f  = Empty
    Branch n x y >>= f = let (Branch n' x' y')  = f n
                         in   Branch n' x' y'

isChildSymmetric :: Tree a -> Bool
isChildSymmetric (Branch a Empty Empty)    = True
isChildSymmetric (Branch a t     Empty)    = False
isChildSymmetric (Branch a Empty t    )    = False
isChildSymmetric (Branch a t0    t1   )    = True

isEndNode :: Tree a -> Bool
isEndNode (Branch a Empty Empty) = True
isEndNode (Branch a t0    t1   ) = False

splitTree :: Tree a -> [Tree a]
splitTree t = if not $ isEndNode t && isChildSymmetric t then
                 let (Branch a t0 t1) = t
                 in  [t0,t1]
              else
                 []

isCompTree :: Tree a -> Bool
isCompTree t = if isChildSymmetric t then
                    let lt = splitTree t
                    in  if length lt == 0 then True
                        else any (\x -> x) $ map isCompTree lt
               else False


isSymmetric :: Tree a -> Tree a -> Tree a -> Bool
isSymmetric _ l r = isMirror l r
    where isMirror Empty Empty = True
          isMirror (Branch _ a b) (Branch _ x y) = isMirror a y && isMirror x b

isSymmetric' :: Tree a -> Bool
isSymmetric' Empty = False
isSymmetric' (Branch _ l r) = isMirror l r
    where isMirror Empty Empty = True
          isMirror (Branch _ a b) (Branch _ x y) = isMirror a y && isMirror x b
          isMirror _ _ = False

compTree' :: Int -> [Tree Int]
compTree' 1 = [leaf 0]
compTree' n = let (patternList,(idxStart,idxEnd)) = treePattern n
              in  map (\x -> compTree'' $ splitRange' (snd $ getRange n) x) patternList
    where compTree'' list = if  length list /= 1 then compTree'' $ tree2TuppleList $ map getNode list
                           else getNode $ head list

treePattern :: Int -> ([[Int]],(Int,Int))
treePattern n = let (s,l) = getRange n
                in  if snd s == n then ([[1]],s)
                    else (combination ([[fst l..snd l] !! i | i <- [0,1..(fst l)-1]]) (n - snd s) , s)

splitRange' :: (Int,Int) -> [Int] -> [(Tree Int, Tree Int)]
splitRange' (f,s) list = let m = (s+1-f) `div` 2
                         in  foldr fnc [] [0..m-1]
    where fnc x ys = let (k,m)   = (f+2*x,f+2*x+1)
                         (k',m') = (k `elem` list, m `elem` list)
                     in  (fnc2 k k',fnc2 m m'):ys
          fnc2 p flg = if flg then
                         Branch p (leaf 0) (leaf 0)
                       else
                         leaf p

getRange :: Int -> ((Int,Int),(Int,Int))
getRange n = getRange' nodeList
    where getRange' (x:y:ys) = 
            if snd x <= n && n < snd y then (x,y)
            else getRange' (y:ys)

tree2TuppleList :: [a] -> [(a,a)]
tree2TuppleList []   = []
tree2TuppleList [x,y] = [(x,y)]
tree2TuppleList list = let m = (length list) `div` 2
                       in  foldr (\x ys -> (list !! (2*x), list !! (2*x+1)):ys) [] [0..m-1]

getNode :: (Tree Int, Tree Int) -> Tree Int
getNode ((Branch x t1 t2),(Branch y t3 t4)) = Branch (x `div` 2) (Branch x t1 t2) (Branch y t3 t4)

nodeList :: [(Int,Int)]
nodeList = let list = map (2^) [0..]
           in zipWith (\i l -> (l,l + (sum $ fst $ splitAt i list))) [0..] list

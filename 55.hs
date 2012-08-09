import MyCombination
import Control.Monad

nodeList :: [(Int,Int)]
nodeList = let list = map (2^) [0..]
           in zipWith (\i l -> (l,l + (sum $ fst $ splitAt i list))) [0..] list

getRange :: Int -> ((Int,Int),(Int,Int))
getRange n = getRange' nodeList
    where getRange' (x:y:ys) = 
            if snd x <= n && n < snd y then (x,y)
            else getRange' (y:ys)

splitRange :: (Int,Int) -> [(Tree Int,Tree Int)]
splitRange (f,s) = let m = (s+1-f) `div` 2
                   in  foldr (\x ys -> (leaf (f+2*x),leaf (f+2*x+1)):ys) [] [0..m-1]

getNode :: (Tree Int, Tree Int) -> Tree Int
getNode ((Branch x t1 t2),(Branch y t3 t4)) = Branch (x `div` 2) (Branch x t1 t2) (Branch y t3 t4)

tree2TuppleList :: [a] -> [(a,a)]
tree2TuppleList []   = []
tree2TuppleList [x,y] = [(x,y)]
tree2TuppleList list = let m = (length list) `div` 2
                       in  foldr (\x ys -> (list !! (2*x), list !! (2*x+1)):ys) [] [0..m-1]

compTree :: Int -> Tree Int
compTree 1 = leaf 0
compTree n = compTree' $ splitRange $ fst $ getRange n
    where compTree' list = if   length list /= 1 then compTree' $ tree2TuppleList $ map getNode list
                           else getNode $ head list

treePattern :: Int -> ([[Int]],(Int,Int))
treePattern n = let (s,l) = getRange n
                in  if snd s == n then ([[1]],s)
                    else (combination [1..(fst l)] (n - snd s) , s)


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

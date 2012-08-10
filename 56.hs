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

isSymmetric :: Tree a -> Bool
isSymmetric t = if isChildSymmetric t then
                    let lt = splitTree t
                    in  if length lt == 0 then True
                        else any (\x -> x) $ map isSymmetric lt
                else False

import Data.Tree

ipl :: Tree a -> Int
ipl (Node n list) = sum $ map calc $ concatMap nrl list

calc :: Int -> Int
calc n = sum [0..n]

nrl :: Tree a -> [Int]
nrl (Node n []) = [1]
nrl (Node n [Node m []]) = [2]
nrl (Node n list) = concatMap (\l -> map (+1) $ (nrl l)) list

--------------------------------------------------------
leaf x = Node x []

tree1 = Node 'a' []

tree2 = Node 'b' [Node 'c' []]

tree3 = Node 'd' [Node 'e' [Node 'f' []]]

tree4 = Node 'g' [Node 'h' [], Node 'i' []]


tree5 = Node 'a' [
          Node 'f' [Node 'g' []],
          Node 'b' [Node 'd' [], Node 'e'[]]
      ]

tree6 = Node 'q' [
            tree1,
            tree2,
            tree3,
            tree4
        ]

tree7 = Node 'x' [
            leaf 'a',
            leaf 'b',
            leaf 'c',
            leaf 'd',
            leaf 'e'
            ]
      
tree8 = Node 'a' [Node 'b' [], Node 'c' [], Node 'd'[Node 'e' [], Node 'f' []]]

--------------------------------------------------------


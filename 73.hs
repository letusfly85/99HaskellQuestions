import Data.Tree

{-
 - data Tree a = Node a [Tree a]
 -     deriving (Eq, Show)
 -}

displayLisp :: Tree Char -> String
displayLisp (Node n [])   = [n]
displayLisp (Node n list) = "(" ++ [n] ++ (concatMap displayLisp' list) ++ ")"
    where displayLisp' (Node n []) = [n]
          displayLisp' (Node n list) = "(" ++ [n] ++ concatMap displayLisp' list ++ ")"

--------------------------------------------------------
leaf x = Node x []

tree1 = Node 'a' []

tree2 = Node 'b' [Node 'c' []]

tree3 = Node 'd' [Node 'e' [Node 'f' []]]

tree4 = Node 'g' [Node 'h' [], Node 'i' []]


tree5 = Node 'a' [
          Node 'f' [Node 'g' []],
          Node 'c' [],
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


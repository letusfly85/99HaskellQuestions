import Data.Tree


bottomUp :: Tree Char -> String
bottomUp (Node c []) = [c]
bottomUp (Node c list)   = (concatMap (\x -> (bottomUp x)) list) ++ [c] 

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


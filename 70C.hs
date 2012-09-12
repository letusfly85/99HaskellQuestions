import Data.Tree

{-
 - data Tree a = Node a [Tree a]
 -     deriving (Eq, Show)
 -}

nnodes :: Tree a -> Int
nnodes (Node n [])     = 1
nnodes (Node n list)   = 1 + sum (map nnodes list)

--------------------------------------------------------
--
tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
          Node 'f' [Node 'g' []],
          Node 'c' [],
          Node 'b' [Node 'd' [], Node 'e' []]
      ]
--
--------------------------------------------------------

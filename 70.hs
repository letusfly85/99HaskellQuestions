import Data.Tree
import Text.Parsec
import Text.Parsec.String

{-
 - data Tree a = Node a [Tree a]
 -     deriving (Eq, Show)
 -}

treeToString :: Tree Char -> String
treeToString (Node c [])     = [c]
treeToString (Node c list)   = [c] ++ (concatMap (\x -> (treeToString x) ++ "^") list)

pNodeParser :: Parser (Tree Char)
pNodeParser = do n <- letter
                 t <- manyTill treeParser endParser
                 return (Node n t)

sNodeParser :: Parser (Tree Char)
sNodeParser = do n <- letter
                 d <- char '^'
                 return (Node n [])

anyParer :: Parser (Tree Char)
anyParer = do t <- try pNodeParser <|> sNodeParser
              return t

treeParser :: Parser (Tree Char)
treeParser = do node   <- letter
                trees  <- try (manyTill anyParer endParser)
                return (Node node trees)

runTreeParser :: String -> IO ()
runTreeParser str = case (parse treeParser "" str) of
                      Right result -> putStrLn $ show result
                      Left  error  -> putStrLn $ show error

--------------------------------------------------------

leaf x = Node x []

tree1 = Node 'a' []

tree2 = Node 'b' [Node 'c' []]

tree3 = Node 'd' [Node 'e' [Node 'f' []]]

tree4 = Node 'g' [Node 'h' [], Node 'i' []]


tree5 = Node 'a' [
          Node 'f' [Node 'g' []],
          tree1,
          tree4
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
str1 = treeToString tree1
str2 = treeToString tree2
str3 = treeToString tree3
str4 = treeToString tree4
str5 = treeToString tree5
str6 = treeToString tree6
str7 = treeToString tree7
str8 = treeToString tree8

--------------------------------------------------------
--
empParer :: Parser (Tree Char)
empParer = do eof
              return (Node '$' [])

dumParser :: Parser (Tree Char)
dumParser = do char '^'
               return (Node '$' [])

endParser :: Parser (Tree Char)
endParser = do try dumParser <|> empParer
               return (Node '$' [])

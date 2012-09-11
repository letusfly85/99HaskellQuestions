{-
 - 2012/09/11
 - https://github.com/letusfly85/99HaskellQuestions/blob/master/69.hs
 -}


import MyTree 

import Text.Parsec hiding (Empty)
import Text.Parsec.String

sampleTree = Branch 'a' (Branch 'g' (Branch 'x' Empty Empty) (Branch 'y' Empty Empty)) (Branch 'c' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty))

tree2Dot :: Tree Char -> String
tree2Dot Empty = "."
tree2Dot (Branch c tl tr) = c : tree2Dot tl ++ tree2Dot tr

tree2Dot' :: Tree Char -> String
tree2Dot' Empty = "."
tree2Dot' (Branch c tl tr) = tree2Dot' tl ++ c : tree2Dot' tr

filterDot :: String -> String
filterDot [] = ""
filterDot (x:xs) = case (== '.') x of
        True   -> filterDot xs
        False  -> x : filterDot xs 

str1 = filterDot $ tree2Dot  sampleTree
str2 = filterDot $ tree2Dot' sampleTree

preInTree :: Monad m => String -> String -> m (Tree Char)
preInTree [] [] = return Empty
preInTree po@(x:xs) io = do (lio,_:rio) <- return $ break (== x) io
                            (lpo,  rpo) <- return $ splitAt (length lio) xs
                            l <- preInTree lpo lio
                            r <- preInTree rpo rio
                            return $ Branch x l r
preInTree _ _ = fail "woops"

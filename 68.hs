{-
 - 2012/09/11
 - https://github.com/letusfly85/99HaskellQuestions/blob/master/68.hs
 -}


import qualified MyTree as M

import Text.Parsec
import Text.Parsec.String
import Data.List (sortBy)

sampleTree = M.Branch 'a' (M.Branch 'g' (M.Branch 'x' M.Empty M.Empty) (M.Branch 'y' M.Empty M.Empty)) (M.Branch 'c' (M.Branch 'd' M.Empty M.Empty) (M.Branch 'e' M.Empty M.Empty))

str1 = treeToPreOrder sampleTree
str2 = treeToInOrder  sampleTree

treeToPreOrder :: M.Tree Char -> String
treeToPreOrder M.Empty = ""
treeToPreOrder (M.Branch n tl tr) = n : treeToPreOrder tl ++ treeToPreOrder tr

treeToInOrder :: M.Tree Char -> String
treeToInOrder M.Empty = ""
treeToInOrder (M.Branch n tl tr) = treeToInOrder tl ++ n : treeToInOrder tr

preInTree :: Monad m => String -> String -> m (M.Tree Char)
preInTree [] [] = return M.Empty
preInTree po@(x:xs) io = do (lio,_:rio) <- return $ break (== x) io
                            (lpo,  rpo) <- return $ splitAt (length lio) xs
                            l <- preInTree lpo lio
                            r <- preInTree rpo rio
                            return $ M.Branch x l r
preInTree _ _ = fail "woops"

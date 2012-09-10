import qualified MyTree as M

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*),(*>),(<$>))

commaParser :: Parser (M.Tree Char)
commaParser = do char ','
                 return (M.Empty)

onePattern :: Parser (M.Tree Char)
onePattern = do l <- letter
                c <- char ','
                return (M.Branch l M.Empty M.Empty)

endPattern :: Parser (M.Tree Char)
endPattern = do l <- letter
                c <- char ')'
                return (M.Branch l M.Empty M.Empty)

endEmptyPattern :: Parser (M.Tree Char)
endEmptyPattern = do char ')'
                     return (M.Empty)

innerPattern :: Parser (M.Tree Char)
innerPattern = do l   <- letter
                  s   <- char '('
                  tl  <- try commaParser <|> onePattern
                  tr  <- try outerPattern <|> leafParser
                  e   <- char ')'
                  c1  <- char ','
                  return (M.Branch l tl tr)

outerPattern :: Parser (M.Tree Char)
outerPattern = do l   <- letter
                  s   <- char '('
                  tl  <- try commaParser <|> onePattern  <|> innerPattern
                  tr  <- try outerPattern <|> leafParser
                  e   <- char ')'
                  return (M.Branch l tl tr)

leafParser :: Parser (M.Tree Char)
leafParser = do l <- letter
                return (M.Branch l M.Empty M.Empty)

leaf :: Char -> M.Tree Char
leaf c = M.Branch c M.Empty M.Empty

parserTree :: Char -> Parser (M.Tree Char)
parserTree c = do s  <- char '('
                  c1 <- try onePattern <|> commaParser  <|> innerPattern
                  c2 <- try endPattern <|> outerPattern <|> endEmptyPattern
                  e  <- char ')'
                  z  <- eof
                  return (M.Branch c c1 c2)

runParserTree :: String -> IO ()
runParserTree str =  let (x, xs) = (head str, tail str)
                     in case (parse (parserTree x) "" xs) of
                          Right tree -> print tree
                          Left  err  -> print err

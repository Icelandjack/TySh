module Parse where

import Text.ParserCombinators.Parsec

import Shell

-- http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-- Better:
-- http://book.realworldhaskell.org/read/using-parsec.html

-- A shell value contains 0 or more commands, each of which is terminated
-- by the pipe | character 
-- shellVal :: GenParser Char st [[String]]
-- shellVal = endBy command pipe

-- Each command contains 1 or more words, separated by spaces
-- command :: GenParser Char st [String]
shellVal = sepBy command pipe
       
-- Build up a list of words
-- command :: GenParser Char st [String]
command = sepBy word spaces --- TODO how to handle trailing whitespace?!

word :: GenParser Char st String
word = many1 alphaNum -- (noneOf "| ")
       
-- The pipe character is |
--pipe :: GenParser Char st Char
pipe =
  do
    char '|'
    spaces

-- Parse a shell value
--parseShellVal :: String -> Either ParseError [[String]]
parseShellVal input = parse shellVal "(unknown)" input

------------------------------------------------------------------------------

-- word :: Parser String
-- word = many1 letter

-- -- sepBy1 word (space <|> char '|')

-- pipeParse = (do
--   skipMany space
--   w <- words
--   skipMany space
--   return w) `sepBy` (char '|')

-- -- readExpr :: String -> IO ()
-- readExpr s = case parse shellParse "<interactive>" s of
--   Left err ->  undefined
--   Right val -> undefined

-- sentence    :: Parser [String]
-- sentence    = do{ words <- sepBy1 word separator
--                 ; oneOf ".?!"
--                 ; return words
--                 }
                
-- separator   :: Parser ()
-- separator   = skipMany1 (space <|> char ',')

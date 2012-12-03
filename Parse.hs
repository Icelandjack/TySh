module Parse where

import Text.ParserCombinators.Parsec

import Shell

-- http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-- spaces :: Parser ()
-- spaces = skipMany1 space

word :: Parser String
word = many1 letter

-- sepBy1 word (space <|> char '|')

pipeParse = (do
  skipMany space
  w <- word
  skipMany space
  return w) `sepBy` (char '|')

shellParse = undefined
  

-- readExpr :: String -> IO ()
readExpr s = case parse shellParse "<interactive>" s of
  Left err ->  undefined
  Right val -> undefined

-- sentence    :: Parser [String]
-- sentence    = do{ words <- sepBy1 word separator
--                 ; oneOf ".?!"
--                 ; return words
--                 }
                
-- separator   :: Parser ()
-- separator   = skipMany1 (space <|> char ',')

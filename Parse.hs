module Parse (parseShellVal) where

import Text.ParserCombinators.Parsec

import Shell

-- http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-- Better:
-- http://book.realworldhaskell.org/read/using-parsec.html

-- Parse a shell value
parseShellVal :: String -> Either ParseError ShellVal
parseShellVal input = case parse shellVal "<interactive>" input of {
  Left x -> Left x ;
  Right strs -> Right $ convert strs
}

-- convert output of successful parse into ShellVal obj
convert :: [[String]] -> ShellVal
convert = Pipe . map convertCommand

convertCommand :: [String] -> Command
convertCommand (c:cs) = Command c cs

-- A shell value contains 0 or more commands separated by the pipe | character
shellVal :: GenParser Char st [[String]]
shellVal = command `sepBy1` (pipe >> spaces)

-- Each command contains 1 or more words separated by spaces
command = word `endBy1` spaces

-- Build up a list of words
word = many1 (noneOf "| ") -- alphaNum
       
-- The pipe character is |
pipe = char '|'


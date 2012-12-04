-- Parse shell commands
module Parse (parseShellVal) where

import Text.ParserCombinators.Parsec
import Prelude hiding (words)
import Shell hiding (pipe)

-- Some useful links:
-- http://legacy.cs.uu.nl/daan/download/parsec/parsec.html
-- http://book.realworldhaskell.org/read/using-parsec.html

type CmdParseObj = ([String], [String])

-- Some valid commands for testing
t1 = "c1 arg arg | c2 | c3 arg"
t2 = "c1 arg arg :: t1 one | c2 :: t2 | c3 arg"

-- Some invalid commands for testing
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"

-- convert output of successful parse into ShellVal obj
convert :: [CmdParseObj] -> ShellVal
-- convert = Pipe . map (\(c:cs, ts) -> Command c cs ts)

convert = Pipe . map convertCommand
convertCommand ((c:cs),[]) = Command c cs []
convertCommand ((c:cs),ts) = Command c cs [unwords ts]

-- A shell value contains 0 or more commands separated by the pipe | character
shellVal :: GenParser Char st [CmdParseObj]
shellVal = command `sepBy1` (pipe >> spaces)

-- Each command contains 1 or more words separated by spaces
-- command = words
command = do
  w1 <- words
  w2 <- option [] typeannot
  return (w1,w2)

typeannot = string "::" >> spaces >> words

-- Build up a list of words
words = word `endBy1` spaces
word = many1 (noneOf "| ::") -- alphaNum
       
-- The pipe character is |
pipe = char '|'

-- Parse a string into shell value, possibly returning a parse error
parseShellVal :: String -> Either ParseError ShellVal
parseShellVal input =
  case parse shellVal "<interactive>" input of 
    Left x -> Left x 
    Right strs -> Right (convert strs)
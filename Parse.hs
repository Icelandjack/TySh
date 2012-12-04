-- Parse shell commands
module Parse (parseInput) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (stringLiteral)
import Prelude hiding (words)
import Shell hiding (pipe)

-- Some useful links:
-- http://legacy.cs.uu.nl/daan/download/parsec/parsec.html
-- http://book.realworldhaskell.org/read/using-parsec.html

type CmdParseObj = ([String], [String])

-- Creates a shell value from parser output
convert :: [CmdParseObj] -> PipeLine
convert = Pipe . map convertCommand

convertCommand ((c:cs),[]) = Command c cs []
convertCommand ((c:cs),ts) = Command c cs [unwords ts]

-- A shell value contains 0 or more commands separated by the pipe | character
pipeLine :: GenParser Char st [CmdParseObj]
pipeLine = command `sepBy1` (pipe >> spaces)

-- Each command contains 1 or more words separated by spaces
-- command = words
command = do
  w1 <- words
  w2 <- option [] typeannot
  return (w1,w2)

-- Type Annotations must be capitalized
typeannot = do
  string "::"
  spaces
  capitalized `endBy1` spaces <?> "Capitalized word"
  where capitalized = upper >>= \a -> many1 letter >>= return . (a:)

-- Build up a list of words
words = word `endBy1` spaces

word = many1 (noneOf "| ::") 
       
-- The pipe character is |
pipe = char '|'

-- Parse a string into shell value, possibly returning a parse error
parseInput :: String -> Either ParseError PipeLine
parseInput input =
  case parse pipeLine "<interactive>" input of 
    Left  err -> Left err
    Right val -> Right (convert val)

------------------------------------------------------------------------------
-- Testing
-- Some valid commands for testing
t1 = "c1 arg arg | c2 | c3 arg"
t2 = "c1 arg arg :: t1 one | c2 :: t2 | c3 arg"

-- Some invalid commands for testing
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"


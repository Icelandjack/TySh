-- Parse shell commands
module Parse (parseShellVal) where

import Text.ParserCombinators.Parsec
import Prelude hiding (words)
import Shell hiding (pipe)

-- Examples ------------------------------------------------------------------

-- valid commands
t1 = "c1 arg arg | c2 | c3 arg"
t2 = "c1 arg arg :: t1 one | c2 :: t2 | c3 arg"

-- invalid commands
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"

------------------------------------------------------------------------------
  
-- Internal type returned from parsing
type CmdParseObj = ([String], [String])

-- Convert output of successful parse into ShellVal obj
convert :: [CmdParseObj] -> ShellVal
convert = Pipe . map convertCommand
convertCommand ((c:cs),[]) = Command c cs []
convertCommand ((c:cs),ts) = Command c cs [unwords ts]

-- A shell value contains 1 or more commands separated by pipes
shellVal :: GenParser Char st [CmdParseObj]
shellVal = command `sepBy1` (pipe >> spaces)
  where pipe = char '|'

-- A command contains 1 or more words separated by spaces
-- optionally followed by a type annotation
command = do
  cmd <- words
  ann <- option [] annotation
  return (cmd, ann)

-- A type annotation is 1 or more words prefixed with a ::
annotation = string "::" >> spaces >> words

-- A list of 1 or more words separated by spaces
words = word `endBy1` spaces
word = many1 (noneOf "| :") -- alphaNum
       
-- Parse a string into shell value, possibly returning parseError
parseShellVal :: String -> Either ParseError ShellVal
parseShellVal input =
  case parse shellVal "<interactive>" input of 
    Left err -> Left err
    Right s -> Right (convert s)

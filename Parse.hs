-- Parse shell commands
module Parse (parseInput) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (stringLiteral)
import Prelude hiding (words)
import Shell hiding (pipe)

-- TODO ----------------------------------------------------------------------

-- Parse stringliterals
-- Change ShellVal to contain [Value] not [String]

-- Examples ------------------------------------------------------------------

type CmdParseObj = ([String], [String])

-- Creates a shell value from parser output
convert :: [CmdParseObj] -> PipeLine
convert = Pipe . map convertCommand

convertCommand (c:cs, ts) = Command c args types
  where
    -- args = map argToValue cs
    args = cs
    types = case ts of {
      [] -> [] ;
      _ -> [unwords ts]
      }

-- argToValue :: String -> Value
-- argToValue s = case reads s :: ([Integer], String) of {
--   (i, "") -> Int i ;
--         _ -> Str s
--   }

-- A shell value contains 0 or more commands separated by the pipe | character
pipeLine :: GenParser Char st [CmdParseObj]
pipeLine = command `sepBy1` (pipe >> spaces)
  where pipe = char '|'

-- A command contains 1 or more words separated by spaces
-- optionally followed by a type annotation
command = do
  cmd <- words
  ann <- option [] annotation
  return (cmd, ann)

-- Type Annotations must be capitalized
annotation = do
  string "::"
  spaces
  capitalized `endBy1` spaces <?> "Capitalized word"
  where capitalized = upper >>= \a -> many1 letter >>= return . (a:)

-- A list of 1 or more words separated by spaces
words = (quoted_string <|> word) `endBy1` spaces
word = many1 (noneOf "| :")
  <?> "Single word"

-- examples: "hello", "hello world"
quoted_string = do
  dquote
  w <- many1 (noneOf "\\\"\r\n")
  dquote
  return w
  <?> "Quoted string"

dquote = char '"' <?> "Double quote"

-- Parse a string into shell value, possibly returning a parse error
parseInput :: String -> Either ParseError PipeLine
parseInput input =
  case parse pipeLine "<interactive>" input of 
    Left  err -> Left err
    Right val -> Right (convert val)

------------------------------------------------------------------------------
-- Testing
-- Some valid commands for testing
t1 = "c1 arg \"hello world\" | c2 | c3 arg"
t2 = "c1 arg arg :: t1 one | c2 :: t2 | c3 arg"

-- Some invalid commands for testing
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"

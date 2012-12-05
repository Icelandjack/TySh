-- Parse shell commands
module Parse (parseInput) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (stringLiteral)
import Prelude hiding (words)
import Shell hiding (pipe)

-- TODO ----------------------------------------------------------------------

-- Change ShellVal to contain [Value] not [String]

-- Examples ------------------------------------------------------------------

type ParsedCommand = ([String], [String]) -- (fun:args, annotation)

-- Create ShellValue from parser output
convert :: [ParsedCommand] -> PipeLine
convert = Pipe . map convertCommand
convertCommand (c:cs, as) = Command c args annot
  where
    -- args = map argToValue cs
    args = cs
    annot = case as of {
      [] -> [] ;
      _ -> [unwords as]
      }

-- argToValue :: String -> Value
-- argToValue s = case reads s :: ([Integer], String) of {
--   (i, "") -> Int i ;
--         _ -> Str s
--   }

-- A shell value contains 0 or more commands separated by the pipe | character
pipeLine :: GenParser Char st [ParsedCommand]
pipeLine =
  command `sepBy1` (pipe >> spaces)
  <?> "pipeline"
  where pipe = char '|'

-- A command contains 1 or more words separated by spaces
-- optionally followed by a type annotation
command = do
  cmd <- words
  ann <- option [] annotation
  return (cmd, ann)
  <?> "command"

-- Type Annotations begin with :: and must be capitalized
annotation = do
  string "::"
  spaces
  capitalized `endBy1` spaces 
  <?> "type annotation"

-- A capitalized word
capitalized = do
  a <- upper
  as <- many1 alphaNum
  return (a:as)
  <?> "capitalized word"

-- A list of 1 or more words/quoted strings
words = (quoted_string <|> word) `endBy1` spaces
  <?> "word list"

-- a continuous sequence of characters (no spaces)
word = many1 (noneOf "| :")
  <?> "single word"

-- a quoted string, eg "hello world"
quoted_string = do
  dquote
  w <- many1 (noneOf "\\\"\r\n")
  dquote
  return w
  <?> "quoted string"
  where dquote = char '"'

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
t2 = "c1 arg arg :: Type One | c2 :: Type2 | c3 arg"

-- Some invalid commands for testing
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"

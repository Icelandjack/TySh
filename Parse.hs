-- Parsing of shell commands

module Parse (parseInput) where

import Text.ParserCombinators.Parsec
import Prelude hiding (words)

import Shell
import Builtin

------------------------------------------------------------------------------
-- Examples
------------------------------------------------------------------------------

-- valid
-- c1 arg "hello world" | c2 | c3 arg
-- c1 arg arg :: Type1 | c2 :: Type2 | c3 arg

-- invalid
-- c1 arg arg | c2 |
-- c1 arg arg :: | c2 :: t2 | c3 arg

------------------------------------------------------------------------------

type ParsedCommand = ([String], [String]) -- (fun:args, annotation)

-- Create PipeLine from parser output
convert :: [ParsedCommand] -> PipeLine
convert = Pipe . map convertCommand

-- Convert successful parse output into Command object
convertCommand :: ParsedCommand -> Command
convertCommand (cmd:args, [])  = Command    cmd (map strToValue args)
convertCommand (cmd:args, ann) = CommandAnn cmd (map strToValue args) (TFun (map TType ann))
convertCommand ([],       _)   = error "Cannot convert empty command"

-- Try to parse a string as Int, otherwise Str
strToValue :: String -> Value
strToValue s =
  case reads s of 
    (i, ""):_ -> Int i
    _         -> Str s

-- A pipeline contains 0 or more commands separated by the pipe | character
pipeLine :: GenParser Char st [ParsedCommand]
pipeLine = command `sepBy1` (pipe >> spaces)
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

-- Parse a string into pipeline, possibly returning a parse error
parseInput :: String -> Either ParseError PipeLine
parseInput input =
  case parse pipeLine "<interactive>" input of 
    Left  err -> Left err
    Right val -> Right (convert val)

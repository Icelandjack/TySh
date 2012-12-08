
-- Parsing of shell commands

module Parse (parseInput) where

import Text.ParserCombinators.Parsec
import Prelude hiding (words)

import Shell (
  Command (Command, CommandAnn),
  PipeLine (Pipe)
  )

import Builtin (
  Value (Int, Str),
  Type (TypeList, Type, TypeVar, Unit)
  )

-- Internal return type from parsing stage
type ParsedCommand = ([String], [String]) -- (fun:args, annotation)

-- Create PipeLine from parser output
convert :: [ParsedCommand] -> PipeLine
convert = Pipe . map convertCommand

-- Convert successful parse output into Command object
convertCommand :: ParsedCommand -> Command
convertCommand (cmd:args, [])  = Command    cmd (map strToValue args)
convertCommand (cmd:args, ann) = CommandAnn cmd (map strToValue args) (TypeList (map Type ann))

strToValue :: String -> Value
strToValue s =
  let parse = reads s :: [(Integer, String)] in
  case parse of {
    (i, ""):_ -> Int i ;
            _ -> Str s
  }

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

------------------------------------------------------------------------------
-- Testing

-- Some valid commands for testing
t1 = "c1 arg \"hello world\" | c2 | c3 arg"
t2 = "c1 arg arg :: Type One | c2 :: Type2 | c3 arg"

-- Some invalid commands for testing
x1 = "c1 arg arg | c2 | "
x2 = "c1 arg arg :: | c2 :: t2 | c3 arg"

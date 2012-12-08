module Builtin where

import Prelude hiding (lookup)

import Data.List hiding (delete, lookup, insert)
import Data.Map hiding (map, foldr)
import Data.IORef

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

-- Type of arguments in the shell
data Value = Int Integer
           | Str String
           | List [Value]

-- A Utility is an inbuilt implementation of a command-line tool
data Utility = Utility {
  fn :: UtilityType,
  typ :: [Type]
}

-- It gets the environment, standard input, arguments and returns a Result
type UtilityType = Env -> String -> [Value] -> IO Result

-- Shell-level type system
data Type = TypeList [Type]     -- [TypeVar "a", TypeVar "b"]: a → b
          | Type String         -- "Tar", "Raw", "PDF", ...
          | TypeVar String      -- a in a → a
          | Unit                -- ()

-- The return type of a utility
data Result = Result {
  out  :: IO String,
  err  :: IO String,
  stat :: IO [ProcessStatus]
}

instance Show Value where
  show (Int a)  = show a
  show (Str s)  = s
  show (List v) = unwords (map show v)

instance Show Type where
  show (Type typ)       = typ
  show (TypeList types) = intercalate " → " (map show types)
  show (TypeVar var)    = var
  show Unit             = "()"

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Mapping from TySh command names to inbuilt utilities,
-- including their in-shell type signatures
builtin = fromList [
    ("typeof", Utility typeof   [])       -- typeof ::

   ,("set",   Utility set   [Unit, Unit])           -- set :: () → ()
   ,("unset", Utility unset [Unit, Unit])           -- unset :: () → ()
   ,("get",   Utility get   [Unit, TypeVar "a"])    -- get :: () → a
   
   -- ,("map",   Utility map'  [])                     -- map :: (a → b) → [a] → [b]
   
   ,("cd",    Utility cd    [Type "String", Unit])  -- cd :: String → ()
   ,("ls",    Utility ls    [Unit, Type "List"])    -- ls :: () → []
   
   ,("cat",   Utility cat   [TypeVar "a", TypeVar "a"])    -- cat :: a → a
   ,("dog",   Utility dog   [TypeVar "a", TypeVar "a"])    -- dog :: a → a
   
   ,("take",  Utility take' [Type "Integer", TypeVar "List", TypeVar "List"])    -- take :: Integer → [a] → [a]
   ]

-- Helpers for constructing return values from utilities
ret out err stat = return (Result (return out) (return err) (return [stat]))
retSuc out       = ret out ""  (Exited ExitSuccess)
retErr err       = ret ""  err (Exited (ExitFailure 2))
void             = retSuc ""

-- Get the type of a utility
typeof _ _ [Str fn] = case lookup fn builtin of
  Just (Utility _ typ) -> retSuc (show typ)
  Nothing -> retErr $ "No built-in utility \"" ++ fn ++ "\""

-- Set an environment variable
set env _ [Str id, val] = setVar env id val >> void
-- set env ""    [Str id]  = unset env "" [Str id] 
set env input [Str id]  = setVar env id (Str input) >> void
set _   _     _         = retErr "usage: set id [value]"

-- Unset an environment variable
unset env input [Str id] = unsetVar env id >> void
unset _   _     _    = retErr "usage: unset id"

-- Get an environment variable
get env _ [Str id] = getVar env id >>= \out -> case out of
  Just value -> retSuc (show value)
  Nothing    -> void
get env _ _    = retErr "usage: get id"

-- Map a function to a list
-- map' env _ [Int 1, _ ] = void

-- Change current working directory
cd env "" []        = getHomeDirectory >>= \dir -> cd env "" [Str dir]
cd env "" [Str dir] = setCurrentDirectory dir >> setVar env "PWD" (Str dir) >> void
cd env input []     = cd env "" [Str input]
cd _ _ _            = retErr "usage: cd [dir]"

-- List files in current directory
ls env _ _ = getCurrentDirectory >>= getDirectoryContents >>= retSuc . unwords . sort

-- Read from file or standard input
cat env input []         = retSuc input
cat env input [Str "-"]  = retSuc input
cat env input [Str file] = readFile file >>= retSuc
cat _   _     _          = retErr "usage: cat FILE"

-- Write to file or standard output
dog env input []         = retSuc input
dog env input [Str "-"]  = retSuc input
dog env input [Str file] = writeFile file input >> void
dog _   _     _          = retErr "usage: dog FILE"

-- Take first n elements from a list
take' _ _ [] = undefined

------------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------------
type Env = IORef (Map String Value)

freshEnv :: IO Env
freshEnv = newIORef empty

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO (Maybe Value)
getVar envRef id = readIORef envRef >>= return . lookup id 
  
setVar :: Env -> String -> Value -> IO Value
setVar envRef id value = do
  env <- readIORef envRef
  writeIORef envRef (insert id value env)
  return value

unsetVar :: Env -> String -> IO ()
unsetVar envRef id = do
  env <- readIORef envRef
  writeIORef envRef (delete id env)
  return ()  

getDefault :: Env -> String -> Value -> IO Value
getDefault envRef id def = do
  env <- readIORef envRef
  return (findWithDefault def id env)

envInit :: Env -> IO ()
envInit env = do
  setVar env "PS1" (Str "TySh>")
  getHomeDirectory >>= setVar env "HOME" . Str
  getEnv "USER"    >>= setVar env "USER" . Str
  getEnv "PATH"    >>= setVar env "PATH" . List . map Str . splitBy ':'
  getEnv "PWD"     >>= setVar env "PWD"  . Str
  return ()

-- From:
-- http://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-string-by-delimiter-funcionally
splitBy del = foldr f [[]] 
  where f c l@(x:xs) | c == del  = []:l
                     | otherwise = (c:x):xs

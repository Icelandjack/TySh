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
type UtilityType = Env -> [Value] -> IO Result

-- The return type of a utility
data Result = Result {
  out  :: IO Value,
  err  :: IO String,
  stat :: IO [ProcessStatus]
}

-- Shell-level type system
data Type = TypeList [Type]     -- [TypeVar "a", TypeVar "b"]: a → b
          | Type String         -- "Tar", "Raw", "PDF", ...
          | TypeVar String      -- a in a → a
          | Unit                -- ()

------------------------------------------------------------------------------

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
    ("typeof", Utility typeof'   [])       -- typeof ::

   ,("set",   Utility set'   [])           -- set :: () → ()
   ,("unset", Utility unset' [])           -- unset :: () → ()
   ,("get",   Utility get'   [])    -- get :: () → a
   
   ,("cd",    Utility cd'    [])  -- cd :: String → ()
   ,("ls",    Utility ls'    [])    -- ls :: () → []
   
   ,("read",   Utility read'   [])    -- cat :: a → a
   ,("write",   Utility write'   [])    -- dog :: a → a
   
   ,("take",  Utility take' [])    -- take :: Integer → [a] → [a]
   ,("drop",  Utility drop' [])    -- drop :: Integer → [a] → [a]
   ]

-- Helpers for constructing return values from utilities
ret out err stat = return (Result (return out) (return err) (return [stat]))
retSuc out       = ret out ""  (Exited ExitSuccess)
retErr err       = ret (Str "")  err (Exited (ExitFailure 2))
void             = retSuc (Str "")

-- Get the type of a utility
typeof' env [Str fn] = case lookup fn builtin of
  Just (Utility _ typ) -> retSuc (Str (show typ))
  Nothing -> retErr $ "No built-in utility \"" ++ fn ++ "\""

-- Set an environment variable
set' env [Str id, val] = setVar env id val >> void
set' _   _             = retErr "usage: set id value"

-- Unset an environment variable
unset' env [Str id] = unsetVar env id >> void
unset' _   _        = retErr "usage: unset id"

-- Get an environment variable
get' env [Str id] = getVar env id >>= \out -> case out of
  Just value -> retSuc value
  Nothing    -> void
get' env _    = retErr "usage: get id"

-- Change current working directory
cd' env []        = getHomeDirectory >>= \dir -> cd' env [Str dir]
cd' env [Str dir] = setCurrentDirectory dir >> setVar env "PWD" (Str dir) >> void
cd' _ _           = retErr "usage: cd [dir]"

-- List files in current directory
-- ls' env _ = getCurrentDirectory >>= getDirectoryContents >>= retSuc . unlines . sort
ls' env _ =
  getCurrentDirectory >>=
  getDirectoryContents >>=
  retSuc . Str . unlines . sort

-- Read from file or standard input
read' env [Str file] = readFile file >>= retSuc . Str
read' _   _          = retErr "usage: read file"

-- Write to file or standard output
write' env [Str file, Str input] = writeFile file input >> void
write' _   _                     = retErr "usage: write file input"

-- Take/drop first n elements from a list
take' env [Int n, List l] = retSuc (List (take n' l))
  where n' = fromInteger n :: Int
drop' env [Int n, List l] = retSuc (List (drop n' l))
  where n' = fromInteger n :: Int

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

module Builtin where

import Prelude hiding (lookup)

import Data.List hiding (delete, lookup, insert)
import Data.Map hiding (map)
import Data.IORef

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

data Value = Int Integer | Str String | List [Value] 

data Result = Result {
  out  :: IO String,
  err  :: IO String,
  stat :: IO [ProcessStatus]
}

-- TODO: Add type classes
--   extraxt ∷ (Archive a) => a -> ()
data Type = Type String         -- "Tar", "Raw", "PDF", ...
          | TypeList [Type]     -- [TypeVar "a", TypeVar "b"]: a → b
          | TypeVar String      -- a in a → a
          | Unit                -- ()

-- A Utility gets the Environment, Input, Arguments and returns the Result
type UtilityType = Env -> String -> [String] -> IO Result

data Utility = Utility {
 fn :: UtilityType,
 typ :: [Type]
}

instance Show Value where
  show (Int a)  = show a
  show (Str s)  = s
  show (List v) = unwords (map show v)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

builtin = fromList [("set",   Utility set [Unit, Unit])
                   ,("unset", Utility unset [Unit, Unit])
                   ,("get",   Utility get [Unit, TypeVar "a"])
                   ,("map",   Utility map' [])
                   ,("cd",    Utility cd [Type "String", Unit])
                   ,("ls",    Utility ls [Unit, Type "List"])
                   ,("cat",   Utility cat [TypeVar "a", TypeVar "a"])]

ret out err stat = return (Result (return out) (return err) (return [stat]))
retSuc out       = ret out ""  (Exited ExitSuccess)
retErr err       = ret ""  err (Exited (ExitFailure 2))
void             = retSuc ""

set env input [id, val] = setVar env id (Str val) >> void
set env ""    [id]      = unset env "" [id] 
set env input [id]      = setVar env id (Str input) >> void
set _   _     _         = retErr "usage: set id [value]"

unset env input [id] = unsetVar env id >> void
unset _   _     _    = retErr "usage: unset id"

get env _ [id] = getVar env id >>= \out -> case out of
  Just value -> retSuc (show value)
  Nothing    -> void
get env _ _    = retErr "usage: get id"

map' env _ ["1", _ ] = void

cd env "" []     = getHomeDirectory >>= \dir -> cd env "" [dir]
cd env "" [dir]  = setCurrentDirectory dir >> setVar env "PWD" (Str dir) >> void
cd env input [] = cd env "" [input]
cd _ _ _        = retErr "usage: cd [dir]"

ls env _ _ = getCurrentDirectory >>= getDirectoryContents >>= retSuc . unwords . sort

cat env input _ = retSuc input

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

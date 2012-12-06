module Builtin where

import Prelude hiding (lookup)

import Data.List hiding (lookup, insert)
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

data Result = Result { out :: IO String, err :: IO String, status :: IO [ProcessStatus] }

data Type = Type String | TypeList [Type] | TypeVar String | Unit

data Utility = Utility {
      fn :: Env -> String -> [String] -> IO Result,
      typ :: [Type]
}

instance Show Value where
  show (Int a)  = show a
  show (Str s)  = s
  show (List v) = unwords (map show v)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

builtin = fromList [("set", Utility set [Unit, Unit])
                   ,("get", Utility get [Unit, TypeVar "a"])
                   ,("map", Utility map' [])
                   ,("cd",  Utility cd [Type "String", Unit])
                   ,("ls",  Utility ls [Unit, Type "List"])]

retSuc out = return (Result (return out) (return "") (return [Exited ExitSuccess]))
retErr err = return (Result (return "") (return err) (return [Exited ExitSuccess]))
void  = retSuc ""

set env input [id, val] = setVar env id (Str val) >> void
set env input [id]      = setVar env id (Str input) >> void
set _ _ _ = retErr "usage: set id [value]"

get env _ [id] = getVar env id >>= \out -> case out of
  Just value -> retSuc (show value)
  Nothing    -> void
get env _ _    = retErr "usage: get id"

map' env _ ["1", _ ] = void

cd env "" []     = getHomeDirectory >>= \dir -> cd env "" [dir]
cd env "" [dir]  = setCurrentDirectory dir >> setVar env "PWD" (Str dir) >> void
cd env input [] = cd env "" [input]
cd _ _ _        = retErr "usage: cd [dir]"

ls env _ _ = getCurrentDirectory >>=
             getDirectoryContents >>=
             ret . unwords . sort

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

getDefault :: Env -> String -> Value -> IO Value
getDefault env id def = do
  val <- getVar env id 
  case val of
    Nothing  -> return def
    Just val -> return val

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

module TySh where

import Prelude hiding (lookup)

import Data.IORef
import Data.Map 
import System.IO
import System.Exit

import Parse
import Shell

type Permissions = (Int,Int,Int)
  
data MimeType = TXT | HTML | PDF | Other

data FileDetails = FileDetails {
  name :: String ,
  path :: FilePath ,
  permissions :: Permissions ,
  size :: Integer ,
  mimetype :: MimeType
  }

type Env = IORef (Map String (IORef String))

prompt :: Env -> IO String
prompt env = do
  putStr "TySh>>> "
  hFlush stdout
  getLine

run :: ShellVal -> IO (ExitCode)
run val = print val >> return ExitSuccess

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO String
getVar envRef id = do
  env <- readIORef envRef
  let (Just val) = lookup id env
  readIORef val

loop :: Env -> IO ()
loop env = do
  input <- prompt env
  -- a :: ShellVal = Pipe [Command]
  -- a <- readExpr input
  shellval <- return (Pipe [Command "ls" [], Command "cat" []])
  ret <- run shellval

  loop env

main :: IO ()
main = do
  env <- newIORef empty
  loop env

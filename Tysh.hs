module TySh where

import Prelude hiding (lookup)

import Data.IORef
import Data.Map 
import System.IO
import System.Exit
import System.Cmd

import System.Console.Haskeline

import Parse
import Shell hiding (run)

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

run :: ShellVal -> IO (ExitCode)
run val = print val >> return ExitSuccess

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO String
getVar envRef id = do
  env <- readIORef envRef
  let (Just val) = lookup id env
  readIORef val

loop :: Env -> InputT IO ()
loop env = do
  minput <- getInputLine "% "
  case minput of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      outputStrLn (show input)
      loop env

  -- input <- prompt env
  -- a :: ShellVal = Pipe [Command]
  -- a <- readExpr input
  -- shellval <- return (Pipe [Command "ls" [], Command "cat" []])
  -- ret <- run shellval

main :: IO ()
main = do
  env <- newIORef empty
  runInputT defaultSettings (loop env)

module TySh where

import Prelude hiding (lookup)

import Data.IORef
import Data.Map 
import System.IO
import System.Exit
import System.Cmd
import Control.Monad.Trans

import System.Console.Haskeline

import Parse (parseShellVal)
import Shell 

data FileDetails = FileDetails {
  name :: String ,
  path :: FilePath ,
  size :: Integer
  }

type Env = IORef (Map String (IORef String))

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO String
getVar envRef id = do
  env <- readIORef envRef
  let (Just val) = lookup id env
  readIORef val

loop :: Env -> InputT IO ()
loop env = do
  input <- getInputLine "% "
  case input of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      case parseShellVal input of
        Left err  -> outputStrLn ("TySh: " ++ show err)
        Right val -> outputStrLn (show val) >> liftIO (run val) >>= outputStrLn
      loop env

  -- shellval <- return (Pipe [Command "ls" [], Command "cat" []])
  -- ret <- run shellval

main :: IO ()
main = do
  env <- newIORef empty
  runInputT defaultSettings (loop env)

module TySh where

import Data.Map
import Data.IORef

import System.Directory
import System.IO
import System.Exit
import System.Cmd
import Control.Monad.Trans

import System.Console.Haskeline

import Parse
import Shell 
import Builtin

loop :: Env -> InputT IO ()
loop env = do
  prompt <- liftIO $ getDefault env "PS1" (Str "%")
  input <- getInputLine (case prompt of { Str s -> s; Int l -> show l; List l -> show l} ++ " ")
  case input of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      case parseInput input of
        Left  err -> outputStrLn ("TySh: " ++ show err)
        Right val -> outputStrLn (show val) >> liftIO (run env val) >>= outputStrLn
      loop env

main :: IO ()
main = do
  env <- freshEnv
  envInit env
  runInputT defaultSettings (loop env)
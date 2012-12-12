module Main where

import Data.Map hiding (filter, map)

import Control.Monad.Trans
import System.Console.Haskeline

import Parse
import Shell 
import Builtin

loop :: Env -> InputT IO ()
loop env = do
  prompt <- liftIO $ getDefault env "PS1" (Str "% ")
  pipe   <- liftIO $ getDefault env "PIPESTATUS" (Str "")
  input  <- getInputLine (show pipe ++ ":" ++ show prompt)
  case input of
    Nothing     -> return ()
    Just "q"    -> return ()
    Just "quit" -> return ()
    Just line  -> do
      case parseInput line of
        Left  err -> outputStrLn ("Input error: " ++ show err)
        Right val -> do -- val :: PipeLine
          stdout <- liftIO (run env val)
          outputStrLn stdout
      loop env

-- Start shell loop with a fresh environment
main :: IO ()
main = do
  env <- freshEnv
  envInit env
  putStrLn "Welcome to TySh! Use 'quit' or 'q' exit the shell."
  runInputT defaultSettings (loop env)

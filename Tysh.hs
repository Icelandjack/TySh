module Main where

import Data.Map hiding (filter, map)
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

-- Pipeline Transformation
--   We transform the pipeline according to the annotations
pipelineTransformation :: PipeLine -> PipeLine
pipelineTransformation pipe = pipe

-- Type erasure
-- TODO: We transform the pipeline according to the annotations
erase :: PipeLine -> PipeLine
erase pipe = pipe

loop :: Env -> InputT IO ()
loop env = do
  prompt <- liftIO $ getDefault env "PS1" (Str "%")
  pipe   <- liftIO $ getDefault env "PIPESTATUS" (Str "")
  input  <- getInputLine (show pipe ++ ":" ++ show prompt ++ " ")
  case input of
    Nothing     -> return ()
    Just "quit" -> return ()
    Just input  -> do
      case parseInput input of
        Left  err -> outputStrLn ("TySh: " ++ show err)
        Right val -> do
          -- Val is a Pipeline
          outputStrLn (show val)

          -- Transform pipeline and erase types
          let process = pipelineTransformation . erase 
          
          -- Run the Pipeline
          stdout <- liftIO $ (run env . process) val
          outputStrLn stdout
      loop env

main :: IO ()
main = do
  env <- freshEnv
  envInit env
  runInputT defaultSettings (loop env)



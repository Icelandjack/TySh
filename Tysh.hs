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
-- TODO: 
--   % cat file ∷ PDF | head
--   → cat file ∷ PDF | pfttotext -layout "$1" - | head

--   % cat file.pdf
--   → cat file.pdf ∷ PDF 

--   % cat file.tar | tr a-z A-Z
--   → cat file.tar ∷ Tar | tar tvf "$1" | tr a-z A-Z
pipelineTransformation :: PipeLine -> PipeLine
pipelineTransformation pipe = pipe

-- Type erasure
-- TODO: We transform the pipeline according to the annotations
erase :: PipeLine -> PipeLine
erase (Pipe cmds) = Pipe (map eraseCmd cmds)
  where
    eraseCmd (CommandAnn cmd args ann) = Command cmd args
    eraseCmd a = a
  
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
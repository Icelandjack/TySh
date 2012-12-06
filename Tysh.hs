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

extensions :: Map String Type
extensions = fromList [
  ("pdf", Type "PDF"),
  ("txt", Type "Text"),
  ("tar", Type "TAR")
  ]

-- TODO: Add type annotations to arguments
-- % cat hi.pdf
-- → cat (hi.pdf ∷ PDF)
addTypes :: Map String Type -> PipeLine -> PipeLine
addTypes ext (Pipe pipe) = Pipe pipe

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
pipelineTransformation (Pipe pipe) = Pipe (transform pipe)
  where transform [] = []
        transform [x] = [x]
        transform ((CommandAnn c as (TypeList a)):xs) = undefined
        transform ((CommandAnn c as (Type a)):xs)     = undefined
        transform ((CommandAnn c as (TypeVar a)):xs)  = undefined
        transform ((CommandAnn c as Unit):xs)         = undefined
        transform ((Command    c as):xs)              = undefined

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

          let process = run env                -- Evaluate the pipeline
                      . erase                  -- Erase types
                      . pipelineTransformation -- Transform the pipeline
                      . addTypes extensions    -- File extensions → types
          
          stdout <- liftIO (process val)
          outputStrLn stdout
      loop env

main :: IO ()
main = do
  env <- freshEnv
  envInit env
  runInputT defaultSettings (loop env)
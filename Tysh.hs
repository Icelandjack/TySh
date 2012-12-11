module Main where

import Data.Map hiding (filter, map)

import Control.Monad.Trans
import System.Console.Haskeline

import Parse
import Shell 
import Builtin

type Ext = Map String Type

extensions :: Ext
extensions = fromList [
  ("pdf", TType "PDF"),
  ("txt", TType "Text"),
  ("tar", TType "TAR")
  ]

-- TODO: Add type annotations to arguments
-- % cat hi.pdf
-- → cat (hi.pdf ∷ PDF)
addTypes :: Ext -> PipeLine -> PipeLine
addTypes e = id
-- addTypes ext (Pipe pipe) = Pipe (map addTypeCom pipe)
--   where
--     addTypeCom :: Command -> Command
--     addTypeCom (CommandAnn cmd args ann) = undefined
--     addTypeCom (Command    cmd args)     = CommandAnn cmd (map (addTypeArg . Arg) args) (TFun [])

--     addTypeArg :: Arg -> Arg
--     addTypeArg (ArgAnn (val, ty)) = undefined
--     addTypeArg (Arg val)          = undefined

-- Pipeline Transformation
--   We transform the pipeline according to the annotations
-- TODO: 
--   % cat file ∷ PDF | head
--   → cat file ∷ PDF | pfttotext -layout "$1" - | head

--   % cat file.pdf
--   → cat (file.pdf ∷ PDF) :: PDF

--   % cat file.tar | tr a-z A-Z
--   → cat file.tar ∷ Tar | tar tvf "$1" | tr a-z A-Z
pipelineTransformation :: PipeLine -> PipeLine
pipelineTransformation = id
-- pipelineTransformation (Pipe pipe) = Pipe (transform pipe)
--   where transform [] = []
--         transform [x] = [x]
--         transform ((CommandAnn c as (TypeList a)):xs) = undefined
--         transform ((CommandAnn c as (Type a)):xs)     = undefined
--         transform ((CommandAnn c as (TypeVar a)):xs)  = undefined
--         transform ((CommandAnn c as Unit):xs)         = undefined
--         transform ((Command    c as):xs)              = undefined

-- Type erasure
-- TODO: We transform the pipeline according to the annotations
erase :: PipeLine -> PipeLine
erase (Pipe cmds) = Pipe (map eraseCmd cmds)
  where
    eraseCmd (CommandAnn cmd args ann) = Command cmd (map argToValue args)
    eraseCmd a = a

-- One loop iteration consists of the following phases:
--   prompt
--   get input
--   parse
--   perform type transformations
--   execute pipeline
loop :: Env -> InputT IO ()
loop env = do
  prompt <- liftIO $ getDefault env "PS1" (Str "%")
  pipe   <- liftIO $ getDefault env "PIPESTATUS" (Str "")
  input  <- getInputLine (show pipe ++ ":" ++ show prompt ++ " ")
  case input of
    Nothing     -> return ()
    Just "q"    -> return ()
    Just "quit" -> return ()
    Just input  -> do
      case parseInput input of
        Left  err -> return () -- outputStrLn ("TySh: " ++ show err)
        Right val -> do -- val :: PipeLine
          -- outputStrLn (show val)
          let process = run env                -- Evaluate the pipeline
                      . erase                  -- Erase types
                      . pipelineTransformation -- Transform the pipeline
                      . addTypes extensions    -- File extensions → types
          
          stdout <- liftIO (process val)
          outputStrLn stdout
      loop env

-- Start shell loop with a fresh environment
main :: IO ()
main = do
  env <- freshEnv
  envInit env
  putStrLn "Welcome to TySh! Use 'quit' or 'q' exit the shell."
  runInputT defaultSettings (loop env)

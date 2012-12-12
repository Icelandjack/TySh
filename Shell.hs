-- Processing of commands and pipelines

module Shell (
  Command(Command, CommandAnn),
  PipeLine(Pipe),
  run
  ) where

import Prelude hiding (lookup)

import Data.Map hiding (map, null)
import Data.List hiding (lookup)
import Control.Monad hiding (void)

import System.Exit
import System.Process (readProcessWithExitCode)

import Builtin

-- A single command in a pipeline, e.g. ls
data Command = CommandAnn String [Value] Type  -- Annotated shell command
             | Command String [Value]          -- Shell command

-- A series of commands, e.g. date | set DATE
data PipeLine = Pipe [Command] 

instance Show Command where
  show (Command    cmd args)           = cmd ++ " " ++ unwords (map show args)
  show (CommandAnn cmd args (TFun [])) = cmd ++ " " ++ unwords (map show args)
  show (CommandAnn cmd args ty)        = show (CommandAnn cmd args (TFun [])) ++ " ∷ " ++ show ty

instance Show PipeLine where
  show (Pipe commands) = intercalate " | " (map show commands)

------------------------------------------------------------------------------
-- Running commands in a pipeline
------------------------------------------------------------------------------

-- Run a pipeline
run :: Env -> PipeLine -> IO String
run env (Pipe cmds) = do
  Result { out = out, err = err, stat = stat } <- invoke' env cmds
  setVar env "PIPESTATUS" (List $ map (Int . fromExitCode) stat)
  when (not (null err)) $ putStrLn err
  return $ show out

-- Invoke a single command
invoke :: Env -> Command -> IO Result
invoke env (CommandAnn cmd args _) = invoke env (Command cmd args)
invoke env (Command cmd args) = 
  case lookup cmd builtin of
    Just (Utility fn _) -> fn env args 
    Nothing -> do
      -- Treat last argument as STDIN
      let (stdin,args') = case length args of 
            0 -> ("",[])
            _ -> (show $ last args, map show $ init args)
      (exitcode, stdout, stderr) <- readProcessWithExitCode cmd args' stdin
      return (Result { out  = Str stdout,
                       err  = stderr,
                       stat = [exitcode] })

-- Invoke first command in a pipe, feeding result to second command
pipe :: Env -> Command -> [Command] -> IO Result
pipe env cmd [] = invoke env cmd
pipe env c1 (c2:cs) = do
  Result { out = out1, err = err1, stat = stat1 } <- invoke' env [c1]
  let c2' = argCurry c2 out1
  Result { out = out2, err = err2, stat = stat2 } <- invoke' env (c2':cs)
  return (Result out2 err2 (stat1 ++ stat2))
  where
    -- Add output of one command to arg list of another
    argCurry :: Command -> Value -> Command
    argCurry (Command fn args) (Str "") = Command fn args 
    argCurry (Command fn args) stdin    = Command fn (args ++ [stdin])

-- Invoke a pipeline by delegating to correct function
invoke' :: Env -> [Command] -> IO Result
invoke' _   []        = void
invoke' env [cmd]     = invoke env cmd
invoke' env (src:dst) = pipe env src dst

-- Convert ExitCode
fromExitCode :: ExitCode -> Integer
fromExitCode (ExitSuccess)   = 0
fromExitCode (ExitFailure n) = fromIntegral n

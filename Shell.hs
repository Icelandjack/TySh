{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- In the use of `catch' (imported from Prelude, but defined in System.IO.Error):
-- Deprecated: "Please use the new exceptions variant, Control.Exception.catch"

-- Processing of commands and pipelines

module Shell (
  Command(Command, CommandAnn),
  PipeLine(Pipe),
  Arg(ArgAnn, Arg, ann, value),
  argToValue,
  run
  ) where

import Prelude hiding (lookup)

import Control.Concurrent
import Control.Monad

import Data.Dynamic
import Data.Map hiding (map)
import Data.List hiding (lookup)
import Data.IORef

import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

import Builtin

type TypeAnnot = Type

data Arg = ArgAnn { ann :: (Value, Type) } | Arg { value :: Value }

argToValue :: Arg -> Value
argToValue (ArgAnn (val, ty)) = val
argToValue (Arg val) = val

-- A single command in a pipeline, e.g. ls
data Command = CommandAnn String [Arg] TypeAnnot -- Annotated shell command
             | Command String [Value]            -- Shell command

-- A series of commands, e.g. date | set DATE
data PipeLine = Pipe [Command] 

-- 
type CloseFDs = MVar [Fd]

instance Show Arg where
  show (ArgAnn (val, ty)) = "(" ++ show val ++ " ∷ " ++ show ty ++ ")"
  show (Arg val) = show val

instance Show Command where
  show (Command    cmd args)               = cmd ++ " " ++ unwords (map show args)
  show (CommandAnn cmd args (TypeList [])) = cmd ++ " " ++ unwords (map show args)
  show (CommandAnn cmd args ty)             = show (CommandAnn cmd args (TypeList [])) ++ " ∷ " ++ show ty

instance Show PipeLine where
  show (Pipe commands) = intercalate " | " (map show commands)

------------------------------------------------------------------------------
-- Running Commands (Adapted from Real-World Haskell)
------------------------------------------------------------------------------

-- Run a pipeline
run :: Env -> PipeLine -> IO String
run env (Pipe cmds) = do
  Result { out = out, err = err, stat = stat } <- invoke' env cmds
  pipestatus <- stat
  setVar env "PIPESTATUS" (List $ map (Int . fromStatus) pipestatus)
  err >>= putStrLn
  out >>= return . show

-- Invoke a single command
invoke :: Env -> Command -> IO Result
invoke env (Command cmd args) = 
  case lookup cmd builtin of
    Just (Utility fn _) -> fn env args 
    Nothing -> error "non-builtin utilities" --- TODO

-- Invoke first command in a pipe, feeding result to second command
pipe :: Env -> Command -> [Command] -> IO Result
pipe env cmd [] = invoke env cmd
pipe env c1 (c2:cs) = do
  Result { out = out1, err = err1, stat = stat1 } <- invoke' env [c1]
  c2' <- fmap (argCurry c1) out1
  Result { out = out2, err = err2, stat = stat2 } <- invoke' env (c2':cs)
  return (Result out2 err2 (liftM2 (++) stat1 stat2))
  where
    argCurry :: Command -> Value -> Command
    argCurry (Command fn args) (Str "") = Command fn args 
    argCurry (Command fn args) stdin    = Command fn (args ++ [stdin])

-- Invoke a pipeline by delegating to correct function
invoke' :: Env -> [Command] -> IO Result
invoke' env [cmd]     = invoke env cmd
invoke' env (src:dst) = pipe env src dst

-- Convert ProcessStatus
fromStatus :: ProcessStatus -> Integer
fromStatus (Exited ExitSuccess)     = 0
fromStatus (Exited (ExitFailure n)) = fromIntegral n
fromStatus _                        = -9999
  
{- 
------------------------------------------------------------------------------
-- Running Commands (Adapted from Real-World Haskell)
------------------------------------------------------------------------------

closeFds = mapM_ (\fd -> catch (closeFd fd) (const (return ())))

invoke env (Command cmd args) closefds input = 
  case lookup cmd builtin of
    Just (Utility fn _) -> fn env input args 
    Nothing -> do
      (r1, w1) <- createPipe -- stdin
      (r2, w2) <- createPipe -- stdout
      (r3, w3) <- createPipe -- stderr
  
      modifyMVar_ closefds $ \old -> return (old ++ [w1, r2, r3])
    
      childPID <- withMVar closefds (\fds -> forkProcess (child (cmd, args) fds r1 w2 w3))
      closeFds [r1, w2, w3]
  
      (stdinh, stdouth, stderrh) <- liftM3 (,,) (fdToHandle w1) (fdToHandle r2) (fdToHandle r3)
  
      forkIO (hPutStr stdinh input >> hClose stdinh)

      return (Result { out  = hGetContents stdouth,
                       err  = hGetContents stderrh,
                       stat = do
                         status <- getProcessStatus True False childPID
                         case status of
                           Nothing -> fail "Error: Nothing from getProcessStatus"
                           Just ps -> removeCloseFDs closefds [w1, r2, r3] >> return [ps]})

child (cmd, args) closefds r1 w2 w3 = do
  dupTo r1 stdInput
  dupTo w2 stdOutput
  dupTo w3 stdError
  closeFds (r1:w2:w3:closefds)
  executeFile cmd True (map show args) Nothing

removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)

    where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance ot any given fd
    removefd [] _ = []
    removefd (x:xs) fd 
        | fd == x = xs
        | otherwise = x : removefd xs fd

pipe :: Env -> Command -> [Command] -> CloseFDs -> String -> IO Result
pipe env src dest closefds input = do
  Result { out = out1, err = err1, stat = stat1 } <- invoke' env [src]  closefds input
  Result { out = out2, err = err2, stat = stat2 } <- invoke' env dest closefds =<< out1

  return (Result out2 err2 (liftM2 (++) stat1 stat2))

invoke' :: Env -> [Command] -> MVar [Fd] -> String -> IO Result
invoke' env [cmd]     fds input = invoke env cmd fds input
invoke' env (src:dst) fds input = pipe env src dst fds input

fromStatus :: ProcessStatus -> Integer
fromStatus (Exited ExitSuccess)   = 0
fromStatus (Exited (ExitFailure n)) = fromIntegral n
fromStatus _ = -9999

run :: Env -> PipeLine -> IO String
run env (Pipe cmds) = do
  closefds <- newMVar []
  Result { out = out, err = err, stat = stat } <- invoke' env cmds closefds []
  pipestatus <- stat
  setVar env "PIPESTATUS" (List $ map (Int . fromStatus) pipestatus)
  err >>= putStrLn
  out

-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- In the use of `catch' (imported from Prelude, but defined in System.IO.Error):
-- Deprecated: "Please use the new exceptions variant, Control.Exception.catch"

module Shell where

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

import Builtin (
  Value (Int, Str, List),
  Type (TypeList, Type, TypeVar, Unit),
  Env,
  Result (Result),
  Utility (Utility),
  builtin, out, err, stat, setVar
  )

type TypeAnnot = Type

-- A single command in a pipeline, e.g. ls
data Command = CommandAnn String [Value] TypeAnnot -- Annotated shell command
             | Command String [Value]              -- Shell command
             -- | CommSub Command                      -- Command substitution
             -- | Value Value                          -- Value

-- A series of commands, e.g. date | tr a-z A-Z
data PipeLine = Pipe [Command] 

-- 
type CloseFDs = MVar [Fd]

instance Show Command where
  show (Command    cmd args)               = cmd ++ " " ++ (unwords (map show args))
  show (CommandAnn cmd args (TypeList [])) = show (Command cmd args)
  show (CommandAnn cmd args ty)            = show (Command cmd args) ++ " ∷ " ++ show ty
  -- show (CommSub command)                  = "$(" ++ show command ++ ")"
  -- show (Value value)                      = show value

instance Show PipeLine where
  show (Pipe commands) = intercalate " | " (map show commands)

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

module Shell where

import Prelude hiding (lookup)

import Control.Concurrent
import Control.Monad

import Data.Dynamic
import Data.Map hiding (map)
import Data.IORef

import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

import Builtin

type TypeAnnot = [String]

data Command = Command String [String] TypeAnnot -- Shell command
             | CommSub Command                   -- Command substitution
             | Value Value                       -- Value
  deriving Show

data PipeLine = Pipe [Command] deriving Show
type CloseFDs = MVar [Fd]

------------------------------------------------------------------------------
-- Runnig Commands (Adapted from RWH)
------------------------------------------------------------------------------

closeFds = mapM_ (\fd -> catch (closeFd fd) (const (return ())))

invoke env (Command cmd args ann) closefds input = 
  case lookup cmd builtin of
    Just fn -> fn env input args 
    Nothing -> do
      (r1, w1) <- createPipe -- stdin
      (r2, w2) <- createPipe -- stdout
      (r3, w3) <- createPipe -- stderr
  
      modifyMVar_ closefds $ \old -> return (old ++ [w1, r2, r3])
    
      childPID <- withMVar closefds (\fds -> forkProcess (child (cmd, args) fds r1 w2 w2))
      closeFds [r1, w2, w2]
  
      (stdinh, stdouth, stderrh) <- liftM3 (,,) (fdToHandle w1) (fdToHandle r2) (fdToHandle r3)
  
      forkIO (hPutStr stdinh input >> hClose stdinh)

      return (Result { out    = hGetContents stdouth,
                       err    = hGetContents stderrh,
                       status = do
                         status <- getProcessStatus True False childPID
                         case status of
                           Nothing -> fail "Error: Nothing from getProcessStatus"
                           Just ps -> removeCloseFDs closefds [w1, r2, r3] >> return [ps]})

child (cmd, args) closefds r1 w2 w3 = do
  dupTo r1 stdInput
  dupTo w2 stdOutput
  dupTo w3 stdError
  closeFds (r1:w2:w3:closefds)
  executeFile cmd True args Nothing

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
  Result { out = out1, err = err1, status = stat1 } <- invoke' env [src]  closefds input
  Result { out = out2, err = err2, status = stat2 } <- invoke' env dest closefds =<< out1

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
  Result { out = output, status = status } <- invoke' env cmds closefds []
  pipestatus <- status
  setVar env "PIPESTATUS" (List $ map (Int . fromStatus) pipestatus)
  output
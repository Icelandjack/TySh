module Shell where

import Control.Concurrent
import Control.Monad

import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

type Arg = String
type TypeAnnot = [String]

data Command = Command String [Arg] TypeAnnot
  deriving Show

data ShellVal = Pipe [Command]
  deriving Show

type CloseFDs = MVar [Fd]

data Result = Result { output :: IO String, stat :: IO [ProcessStatus] }

closeFds = mapM_ (\fd -> catch (closeFd fd) (const (return ())))

invoke (Command cmd args _) closefds input = do
  (r1, w1) <- createPipe
  (r2, w2) <- createPipe
  modifyMVar_ closefds (\old -> return (old ++ [w1, r2]))
  childPID <- withMVar closefds (\fds -> forkProcess (child (cmd, args) fds r1 w2))
  closeFds [r1, w2]
  (stdinhdl, stdouthdl) <- liftM2 (,) (fdToHandle w1) (fdToHandle r2)
  forkIO (hPutStr stdinhdl input >> hClose stdinhdl)
  let exit = do
        status <- getProcessStatus True False childPID
        case status of
          Nothing -> fail "Error: Nothing from getProcessStatus"
          Just ps -> removeCloseFDs closefds [w1, r2] >> return [ps]

  return (Result { output = hGetContents stdouthdl, stat = exit })

child (cmd, args) closefds r1 w2 = do
  dupTo r1 stdInput
  dupTo w2 stdOutput
  closeFds (r1:w2:closefds)
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

--  (closefds :: MVar [Fd]) <- newMVar []
pipe src dest closefds input = do
  Result { output = output1, stat = status1 } <- invoke' [src]  closefds input
  Result { output = output2, stat = status2 } <- invoke' dest closefds =<< output1

  return (Result output2 (liftM2 (++) status1 status2))

invoke' [cmd] fds input = invoke cmd fds input
invoke' (src:dst) fds input = pipe src dst fds input

run :: ShellVal -> IO String
run (Pipe cmds) = do
  closefds <- newMVar []
  Result { output = output, stat = status } <- invoke' cmds closefds []  
  output

-- command = [Command "ls" [] []]
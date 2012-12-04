module Shell where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Control.Monad

type Arg = String

newtype Annotation = Annotation String deriving Show

data Command = Command String [Arg] Annotation 
  deriving Show

data ShellVal = Pipe [Command]
  deriving Show

data Result = Result { output :: IO String, status :: IO [ProcessStatus] }

type CloseFDs = MVar [Fd]

closeFds = mapM_ (\fd -> catch (closeFd fd) (const (return ())))

invoke (cmd, args) closefds input = do
  (r1, w1) <- createPipe
  (r2, w2) <- createPipe
  
  modifyMVar_ closefds (\old -> return (old ++ [w1, r2]))
  
  childPID <- withMVar closefds (\fds -> forkProcess (child (cmd, args) fds r1 w2))
  
  closeFds [r1, w2]

  (stdinhdl, stdouthdl) <- liftM2 (,) (fdToHandle w1) (fdToHandle r2)
  
  forkIO (hPutStr stdinhdl input >> hClose stdinhdl)

  let output = hGetContents stdouthdl
      exit   = do
        status <- getProcessStatus True False childPID
        case status of
          Nothing -> fail "Error: Nothing from getProcessStatus"
          Just ps -> removeCloseFDs closefds [w1, r2] >> return [ps]

  return (Result { output = output, status = exit })

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
  Result { output = output1, status = status1 } <- invoke' [src]  closefds input
  Result { output = output2, status = status2 } <- invoke' dest closefds =<< output1

  return (Result output2 (liftM2 (++) status1 status2))

invoke' [cmd] fds input = invoke cmd fds input
invoke' (src:dst) fds input = pipe src dst fds input

run cmd = do
  closefds <- newMVar []
  Result { output = output, status = status } <- invoke' cmd closefds []  
  status >>= putStrLn . show
  output >>= putStr


commands = [("/bin/ls",[]), ("/bin/cat",[])]

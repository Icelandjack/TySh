module Builtin where

import Prelude hiding (lookup)

import Data.Map hiding (map)
import Data.IORef

import System.Directory
import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

data Value = I Integer | S String | L [Value] 

data Result = Result { out :: IO String, err :: IO String, status :: IO [ProcessStatus] }

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

builtin = fromList [("set", set)
                   ,("get", get)
                   ,("map", map')
                   ,("cd",  cd)
                   ,("ls",  ls)]

ret s = return (Result (return s) (return "") (return [Exited ExitSuccess]))
void  = ret ""

set env _ [id, val] = setVar env id (S val) >> void
set env _  _        = void

get env _ [id] = getVar env id >>= \out -> ret (show out)
get env _ _    = void


map' env _ ["1", _ ] = void

cd env _ []    = getHomeDirectory >>= \dir -> cd env undefined  [dir]
cd env _ [dir] = setCurrentDirectory dir >> void

ls env = undefined

------------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------------
type Env = IORef (Map String Value)

instance Show Value where
  show (I a) = show a
  show (S s) = s
  show (L v) = show v

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO (Maybe Value)
getVar envRef id = readIORef envRef >>= return . lookup id 
  
setVar :: Env -> String -> Value -> IO Value
setVar envRef id value = do
  env <- readIORef envRef
  writeIORef envRef (insert id value env)
  return value

getDefault :: Env -> String -> Value -> IO Value
getDefault env id def = do
  val <- getVar env id 
  case val of
    Nothing  -> return def
    Just val -> return val


-- Built-in utilities and handling of environments

module Builtin (
  Value (Int, Str, List),
  Type (TFun, TType, TVar, TUnit, TString, TInt, TList),
  Env,
  Result (Result),
  Utility (Utility),
  getDefault, freshEnv, envInit,
  builtin, out, err, stat, setVar
  ) where

import Prelude hiding (lookup)

import Data.List hiding (delete, lookup, insert)
import Data.Map hiding (map, foldr)
import Data.IORef

import System.Directory
import System.Environment
import System.Exit

-- Type of arguments in the shell
data Value = Int Integer
           | Str String
           | List [Value]
           deriving (Eq, Ord)

-- A Utility is an inbuilt implementation of a command-line tool
data Utility = Utility {
  fn :: UtilityType,
  typ :: [Type]
}

-- It gets the environment, standard input, arguments and returns a Result
type UtilityType = Env -> [Value] -> IO Result

-- The return type of a utility
data Result = Result {
  out  :: Value,
  err  :: String,
  stat :: [ExitCode]
}

-- Shell-level type system
data Type = TFun [Type]     -- [TypeVar "a", TypeVar "b"]: a → b
          | TType String     -- "Tar", "Raw", "PDF", ...
          | TVar String      -- a in a → a
          | TUnit            -- ()
          | TString          -- Str
          | TInt             -- Int
          | TList            -- List

------------------------------------------------------------------------------

instance Show Value where
  show (Int a)  = show a
  show (Str s)  = s
  show (List v) = unwords (map show v)

instance Show Type where
  show (TType typ)    = typ
  show (TFun types)  = intercalate " → " (map show types)
  show (TVar var)     = var
  show (TString)      = "String"
  show (TInt)         = "Int"
  show (TList)        = "List"
  show TUnit          = "()"

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Mapping from TySh command names to inbuilt utilities,
-- including their in-shell type signatures
builtin = fromList [
  -- typeof :: String → String
  ("typeof", Utility typeof' [TString, TString])

  -- set :: String → a → ()
  ,("set",   Utility set'   [TString, TVar "a", TUnit])

  -- unset :: String → ()    
  ,("unset", Utility unset' [TString, TUnit])

  -- get :: String → a
  ,("get",   Utility get'   [TString, TVar "a"])

  -- cd :: String → ()
  ,("cd",    Utility cd'    [TString, TUnit])

  -- ls :: () → []
  ,("ls",    Utility ls'    [TUnit, TList])

  -- read :: String → String
  ,("read",  Utility read'  [TString, TString])

  -- write :: String → String → ()
  ,("write", Utility write' [TString, TString, TUnit])
  
  -- sort :: [a] → [a]
  ,("sort",  Utility sort'  [TList, TList])
   
  -- pick :: [a] → Integer → a
  ,("pick",  Utility pick'  [TList, TInt, TVar "a"])
   
  -- take :: Integer → [a] → [a]
  ,("take",  Utility take'  [TInt, TList, TList])
   
  -- drop :: Integer → [a] → [a]
  ,("drop",  Utility drop'  [TInt, TList, TList])
  ]

-- Helpers for constructing return values from utilities
-- ret out err stat = return (Result (return out) (return err) (return [stat]))
ret out err stat = return (Result out err [stat])
retSuc out       = ret out ""  (ExitSuccess)
retErr err       = ret (Str "")  err (ExitFailure 2)
void             = retSuc (Str "")

-- Get the type of a utility
typeof' env (Str fn : _) = case lookup fn builtin of
  Just (Utility _ typ) -> retSuc $ Str (show typ)
  Nothing -> retSuc $ Str ("String → String (default)")
typeof' env _            = retErr "usage: typeof UTILITY"

-- Set an environment variable
set' env (Str id : val : _) = setVar env id val >> void
set' env _                  = retErr "usage: set ID VALUE"

-- Unset an environment variable
unset' env (Str id : _) = unsetVar env id >> void
unset' env _            = retErr "usage: unset ID"

-- Get an environment variable
get' env (Str id : _) = getVar env id >>= \out -> case out of
  Just value -> retSuc value
  Nothing    -> void
get' env _            = retErr "usage: get ID"

-- Change current working directory
cd' env []            = getHomeDirectory >>= \dir -> cd' env [Str dir]
cd' env (Str dir : _) = setCurrentDirectory dir >> setVar env "PWD" (Str dir) >> void
cd' env _             = retErr "usage: cd [DIR]"

-- List files in current directory
-- ls' env _ = getCurrentDirectory >>= getDirectoryContents >>= retSuc . unlines . sort
ls' env _ =
  getCurrentDirectory >>=
  getDirectoryContents >>=
  retSuc . List . map Str

-- Read from file or standard input
read' env (Str file : _) = readFile file >>= retSuc . Str
read' env _              = retErr "usage: read FILE"

-- Write to file or standard output
write' env (Str file : Str input : _) = writeFile file input >> void
write' env _                          = retErr "usage: write file input"

-- Sort a list
sort' env (List l : _) = retSuc (List (sort l))
sort' env _            = retErr "usage: sort LIST"

-- Pick item n from a list
pick' env (Int n : List l : _) = retSuc (l !! n')
  where n' = fromInteger n :: Int
pick' env _                    = retErr "usage: pick N LIST"

-- Take first n elements from a list
take' env (Int n : List l : _) = retSuc (List (take n' l))
  where n' = fromInteger n :: Int
take' env _                    = retErr "usage: take N LIST"

-- Drop first n elements from a list
drop' env (Int n : List l : _) = retSuc (List (drop n' l))
  where n' = fromInteger n :: Int
drop' env _                    = retErr "usage: drop N LIST"

------------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------------

type Env = IORef (Map String Value)

freshEnv :: IO Env
freshEnv = newIORef empty

isBound :: Env -> String -> IO Bool
isBound envRef id = readIORef envRef >>= return . member id 

getVar :: Env -> String -> IO (Maybe Value)
getVar envRef id = readIORef envRef >>= return . lookup id 
  
setVar :: Env -> String -> Value -> IO Value
setVar envRef id value = do
  env <- readIORef envRef
  writeIORef envRef (insert id value env)
  return value

unsetVar :: Env -> String -> IO ()
unsetVar envRef id = do
  env <- readIORef envRef
  writeIORef envRef (delete id env)
  return ()  

getDefault :: Env -> String -> Value -> IO Value
getDefault envRef id def = do
  env <- readIORef envRef
  return (findWithDefault def id env)

envInit :: Env -> IO ()
envInit env = do
  setVar env "PS1" (Str "TySh>")
  getHomeDirectory >>= setVar env "HOME" . Str
  getEnv "USER"    >>= setVar env "USER" . Str
  getEnv "PATH"    >>= setVar env "PATH" . List . map Str . splitBy ':'
  getEnv "PWD"     >>= setVar env "PWD"  . Str
  return ()

-- From:
-- http://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-string-by-delimiter-funcionally
splitBy del = foldr f [[]] 
  where f c l@(x:xs) | c == del  = []:l
                     | otherwise = (c:x):xs

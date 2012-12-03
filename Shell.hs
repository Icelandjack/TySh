module Shell where

type Arg = String

data Command = Command String [Arg]
  deriving Show

data ShellVal = Pipe [Command]
  deriving Show

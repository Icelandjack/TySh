module Shell where

type Arg = String

type TypeAnnot = Maybe String

data Command = Command String [Arg] TypeAnnot
  deriving Show

data ShellVal = Pipe [Command]
  deriving Show

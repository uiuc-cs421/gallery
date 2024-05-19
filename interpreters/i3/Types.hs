module Types where

  data Exp = IntExp Integer
           | IntOpExp String Exp Exp
           | IntCompExp String Exp Exp
           | BoolOpExp String Exp Exp
           | BoolExp Bool
     deriving (Show, Eq)

  data Val = IntVal Integer
           | BoolVal Bool
     deriving (Show, Eq)

  type Env = [(String,Val)]

  -- Represent the empty environment

  emptyEnv :: Env
  emptyEnv = []

  -- To insert a value into an environment

  insert k v env = (k,v):env

module Main where

import Types
import Parser
import Text.Megaparsec
import System.Console.Haskeline

intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)]

boolOps = [ ("&&",(&&))
          , ("||",(||))]

relOps = [ ("<", (<))
         , ("<=", (<=))
         , (">", (>))
         , (">=", (>=))
         , ("==", (==))
         , ("/=", (/=)) ]

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

liftRelOp f (IntVal i1) (IntVal i2) = BoolVal (f i1 i2)
liftRelOp f _           _           = BoolVal False

liftBoolOp f (BoolVal i1) (BoolVal i2) = BoolVal (f i1 i2)
liftBoolOp f _            _            = BoolVal False


eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (BoolExp b) _ = BoolVal b

eval (BoolOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op boolOps
   in liftBoolOp f v1 v2

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2

eval (RelOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op relOps
   in liftRelOp f v1 v2

eval (VarExp var) env =
   case lookup var env of
      Just val -> val
      Nothing -> IntVal 0

eval (LetExp var e1 e2) env =
  let v1 = eval e1 env
   in eval e2 (insert var v1 env)

repl :: Env -> IO ()
repl env = runInputT defaultSettings loop
  where loop = do minput <- getInputLine "i4> "
                  case minput of
                    Nothing -> return ()
                    Just "quit" -> return ()
                    Just input -> do case parse mainParser "<stdin>" input of
                                       Right exp -> outputStrLn (show $ eval exp env)
                                       Left msg -> outputStrLn (show msg)
                                     loop

main :: IO ()
main = do
   putStrLn "Welcome to your interpreter!"
   repl [("x",IntVal 3)]

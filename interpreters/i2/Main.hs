module Main where

import Types
import Parser
import Text.Megaparsec
import System.Console.Haskeline

intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)]

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
   in liftIntOp f v1 v2

repl :: Env -> IO ()
repl env = runInputT defaultSettings loop
  where loop = do minput <- getInputLine "i2> "
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
   repl emptyEnv

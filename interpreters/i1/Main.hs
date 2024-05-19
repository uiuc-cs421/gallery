module Main where

import Types
import Parser
import Text.Megaparsec
import System.Console.Haskeline

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i

repl :: Env -> IO ()
repl env = runInputT defaultSettings loop
  where loop = do minput <- getInputLine "i1> "
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

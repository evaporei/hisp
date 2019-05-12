module Repl
    ( repl
    ) where

import System.IO (hFlush, stdout)

import Err (Err(..))
import Env (Env)
import Tokenize (tokenize)
import Parse (parse)
import Eval (eval)

repl :: Env -> IO ()
repl env = do
  putStr "hisp > "
  hFlush stdout
  input <- getLine
  if input == ".exit"
     then return ()
     else case (parse (tokenize input)) of
            Left err -> print (reason err) >> repl env
            Right (expr, _) -> case (eval env expr) of
                                 Left l -> print l >> repl env
                                 Right (newEnv, expr) -> print expr >> repl newEnv

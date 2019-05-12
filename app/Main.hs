module Main where

import Repl (repl)
import Env (defaultEnv)

main :: IO ()
main = do
  repl defaultEnv

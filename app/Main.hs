module Main where

import Lib (repl, defaultEnv)

main :: IO ()
main = do
  repl defaultEnv

import System.IO (hFlush, stdout)

data Expr = Symbol String | Number Float | List [Expr]

repl = do
  putStr "hisp > "
  hFlush stdout
  input <- getLine
  if input == ".exit"
     then return ()
     else match_input input >> repl

match_input :: String -> IO ()
match_input input
  | otherwise = return ()

main = do
  repl

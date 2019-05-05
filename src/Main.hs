import System.IO (hFlush, stdout)
import Data.Map (Map)

data Expr = Symbol String | Number Float | List [Expr]

data Err = Err { reason :: String }

data Env = Env { data' :: (Map String Expr) }

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

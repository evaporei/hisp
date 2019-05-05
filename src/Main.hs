import System.IO (hFlush, stdout)
import Data.Map (Map)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)

-- pipeline operator
x |> f = f x

data Expr = Symbol String | Number Float | List [Expr]

data Err = Err { reason :: String }

data Env = Env { data' :: (Map String Expr) }

tokenize :: String -> [String]
tokenize expr = expr
  |> replace "(" " ( "
  |> replace ")" " ) "
  |> splitOn " "
  |> filter (not . null)

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

import System.IO (hFlush, stdout)
import Data.Map (Map)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- pipeline operator
x |> f = f x

data Expr = Symbol String | Number Float | List [Expr]
  deriving(Show)

data Err = Err { reason :: String }
  deriving(Show)

data Env = Env { data' :: (Map String Expr) }

tokenize :: String -> [String]
tokenize expr = expr
  |> replace "(" " ( "
  |> replace ")" " ) "
  |> splitOn " "
  |> filter (not . null)

parse :: [String] -> Either Err (Expr, [String])
parse [] = Left Err { reason = "Could not get token, string is empty" }
parse (token:rest)
  | token == "(" = read_seq rest []
  | token == ")" = Left Err { reason = "Unexpected `)` token" }
  | otherwise = Right ((parse_atom token), rest)

read_seq :: [String] -> [Expr] -> Either Err (Expr, [String])
read_seq [] _ = Left Err { reason = "Could not find closing `)`" }
read_seq (next_token:rest) expr_list
  | next_token == ")" = Right (List expr_list, rest)
  | otherwise = case (parse ([next_token] ++ rest)) of
                  Right (expr, tokens) -> read_seq tokens (expr_list ++ [expr])
                  Left err -> Left err

parse_atom :: String -> Expr
parse_atom atom = case (readMaybe atom) of
                    Nothing -> Symbol atom
                    Just v -> Number v

repl = do
  putStr "hisp > "
  hFlush stdout
  input <- getLine
  if input == ".exit"
     then return ()
     else print (parse (tokenize input)) >> repl

main = do
  repl

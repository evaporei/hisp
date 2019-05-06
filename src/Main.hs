import System.IO (hFlush, stdout)
import Data.Map (Map, fromList, lookup)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Text.Show.Functions
import Data.Either (lefts, isLeft, rights)

-- pipeline operator
x |> f = f x

data Expr = Symbol String
          | Number Float
          | List [Expr]
          | Func ([Expr] -> Either Err Expr)
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

is_number :: Expr -> Bool
is_number expr = case expr of
                   Number n -> True
                   _ -> False

sum'_aux :: Float -> Expr -> Float
sum'_aux acc expr = case expr of
                   Number n -> acc + n
                   _ -> error "Should not sum Expressions that are not Numbers"

sum' :: [Expr] -> Either Err Expr
sum' [] = Left Err { reason = "Could not sum, list expression is empty" }
sum' expr_list = case (any is_number expr_list) of
                   False -> Left Err { reason = "Could not sum, not all expressions in list are Numbers" }
                   True -> Right (Number (foldl sum'_aux 0.0 expr_list))

get_expr_number :: Expr -> Float
get_expr_number expr = case expr of
                         Number n -> n
                         _ -> error "Could not get Number out of an Expression that is not a Number"

subtract' :: [Expr] -> Either Err Expr
subtract' [] = Left Err { reason = "Could not subtract, list expression is empty" }
subtract' (x:xs) = case (any is_number ([x] ++ xs)) of
                   False -> Left Err { reason = "Could not subtract, not all expressions in list are Numbers" }
                   True -> Right (Number ((get_expr_number x) - (foldl sum'_aux 0.0 xs)))

default_env = Env {
  data' = Data.Map.fromList [
    ("+", Func sum'),
    ("-", Func subtract')
                            ]
                  }

call_eval_on_arg :: Env -> Expr -> Either Err Expr
call_eval_on_arg env expr = eval env expr

eval :: Env -> Expr -> Either Err Expr
eval env expr = case expr of
                  Symbol s -> case Data.Map.lookup s (data' env) of
                                Nothing -> Left Err { reason = "Unexpected symbol: " ++ s }
                                Just v -> Right v
                  Number n -> Right expr
                  List list -> case (null list) of
                                 True -> Left Err { reason = "Expected a non empty list" }
                                 False -> case (eval env (head list)) of
                                            Left err -> Left err
                                            Right expr -> case expr of
                                                            Func func -> let eval_args = (map (call_eval_on_arg env) (tail list))
                                                                          in case (any isLeft eval_args) of
                                                                               True -> Left (head (lefts eval_args))
                                                                               False -> func (rights eval_args)
                                                            _ -> Left Err { reason = "First form must be a function" }
                  Func _ -> Left Err { reason = "Unexpected form" }


repl = do
  putStr "hisp > "
  hFlush stdout
  input <- getLine
  if input == ".exit"
     then return ()
     else case (parse (tokenize input)) of
            Left err -> print (reason err) >> repl
            Right (expr, _) -> print (eval default_env expr) >> repl

main = do
  repl

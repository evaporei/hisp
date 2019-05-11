import System.IO (hFlush, stdout)
import Data.Map (Map, fromList, lookup, insert)
import Data.List (intercalate)
import Text.Read (readMaybe)
import Text.Show.Functions
import Data.Either (lefts, isLeft, rights)
import Data.Char (toLower)
import qualified Control.Exception as Exc
import System.IO.Unsafe

-- pipeline operator
x |> f = f x

data Expr = Boolean Bool
          | Symbol String
          | Number Float
          | List [Expr]
          | Func ([Expr] -> Either Err Expr)
          | Lambda LambdaData

instance Show Expr where
  show e = case e of
             Boolean b -> map toLower (show b)
             Symbol s -> s
             Number n -> show n
             List list -> "(" ++ (intercalate "," (map show list)) ++ ")"
             Func f -> show f
             Lambda l -> show l

instance Eq Expr where
  (Boolean x) == (Boolean y) = x == y
  (Symbol x) == (Symbol y) = x == y
  (Number x) == (Number y) = x == y
  (List x) == (List y) = x == y
  (Func x) == (Func y) = error "Should not compare equality of functions"
  _ == _ = error "Should not compare of equality different types"

instance Ord Expr where
  (Boolean x) `compare` (Boolean y) = error "Should not compare booleans"
  (Symbol x) `compare` (Symbol y) = error "Should not compare symbols"
  (Number x) `compare` (Number y) = x `compare` y
  (List x) `compare` (List y) = error "Should not compare lists"
  (Func x) `compare` (Func y) = error "Should not compare functions"
  _ `compare` _ = error "Should not compare different types"

data Err = Err { reason :: String }

instance Show Err where
  show e = "Error: " ++ (reason e)

data Env = Env { data' :: (Map String Expr) }

data LambdaData = LambdaData { params :: Expr, body :: Expr }

instance Show LambdaData where
  show l = "<lambda>"

-- adapted from: http://bluebones.net/2007/01/replace-in-haskell/
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ (replace find repl (drop (length find) s))
        else [head s] ++ (replace find repl (tail s))

tokenize :: String -> [String]
tokenize expr = expr
  |> replace "(" " ( "
  |> replace ")" " ) "
  |> words

parse :: [String] -> Either Err (Expr, [String])
parse [] = Left Err { reason = "Could not get token, string is empty" }
parse (token:rest)
  | token == "(" = readSeq rest []
  | token == ")" = Left Err { reason = "Unexpected `)` token" }
  | otherwise = Right ((parseAtom token), rest)

readSeq :: [String] -> [Expr] -> Either Err (Expr, [String])
readSeq [] _ = Left Err { reason = "Could not find closing `)`" }
readSeq (nextToken:rest) exprList
  | nextToken == ")" = Right (List exprList, rest)
  | otherwise = case (parse ([nextToken] ++ rest)) of
                  Right (expr, tokens) -> readSeq tokens (exprList ++ [expr])
                  Left err -> Left err

parseAtom :: String -> Expr
parseAtom atom 
  | atom == "true" = Boolean True
  | atom == "false" = Boolean False
  | otherwise = case (readMaybe atom) of
                  Nothing -> Symbol atom
                  Just v -> Number v

isNumber :: Expr -> Bool
isNumber expr = case expr of
                   Number n -> True
                   _ -> False

sum'Aux :: Float -> Expr -> Float
sum'Aux acc expr = case expr of
                   Number n -> acc + n
                   _ -> error "Should not sum Expressions that are not Numbers"

sum' :: [Expr] -> Either Err Expr
sum' [] = Left Err { reason = "Could not sum, list expression is empty" }
sum' exprList = case (any isNumber exprList) of
                   False -> Left Err { reason = "Could not sum, not all expressions in list are Numbers" }
                   True -> Right (Number (foldl sum'Aux 0.0 exprList))

getExprNumber :: Expr -> Float
getExprNumber expr = case expr of
                         Number n -> n
                         _ -> error "Could not get Number out of an Expression that is not a Number"

subtract' :: [Expr] -> Either Err Expr
subtract' [] = Left Err { reason = "Could not subtract, list expression is empty" }
subtract' (x:xs) = case (any isNumber ([x] ++ xs)) of
                   False -> Left Err { reason = "Could not subtract, not all expressions in list are Numbers" }
                   True -> Right (Number ((getExprNumber x) - (foldl sum'Aux 0.0 xs)))

-- copied from: https://stackoverflow.com/questions/6121256/efficiently-checking-that-all-the-elements-of-a-big-list-are-the-same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

-- copied from: https://stackoverflow.com/questions/27392547/return-the-first-line-of-a-string-in-haskell
firstLine :: String -> String
firstLine = head . lines

-- adapted from: https://stackoverflow.com/questions/4243117/how-to-catch-and-ignore-a-call-to-the-error-function
unsafeBoolCleanup :: Bool -> Either Err Expr
unsafeBoolCleanup x = unsafePerformIO $ Exc.catch (x `seq` return (Right (Boolean x))) handler
    where
      handler exc = return (Left Err { reason = (firstLine $ show exc) }) `const`  (exc :: Exc.ErrorCall)

equal' :: [Expr] -> Either Err Expr
equal' [] = Left Err { reason = "Could not compare equality, list expression is empty" }
equal' exprList = unsafeBoolCleanup $ allTheSame exprList

allGreater :: (Ord a) => [a] -> Bool
allGreater [] = True
allGreater [x] = True
allGreater (x:y:xs) = x > y && allGreater (y:xs)

greaterThan' :: [Expr] -> Either Err Expr
greaterThan' [] = Left Err { reason = "Could not compare, list expression is empty" }
greaterThan' exprList = unsafeBoolCleanup $ allGreater exprList

allLess :: (Ord a) => [a] -> Bool
allLess [] = True
allLess [x] = True
allLess (x:y:xs) = x < y && allLess (y:xs)

lessThan' :: [Expr] -> Either Err Expr
lessThan' [] = Left Err { reason = "Could not compare, list expression is empty" }
lessThan' exprList = unsafeBoolCleanup $ allLess exprList

allGreaterThanOrEqual :: (Ord a) => [a] -> Bool
allGreaterThanOrEqual [] = True
allGreaterThanOrEqual [x] = True
allGreaterThanOrEqual (x:y:xs) = x >= y && allGreaterThanOrEqual (y:xs)

greaterThanOrEqual' :: [Expr] -> Either Err Expr
greaterThanOrEqual' [] = Left Err { reason = "Could not compare, list expression is empty" }
greaterThanOrEqual' exprList = unsafeBoolCleanup $ allGreaterThanOrEqual exprList

allLessThanOrEqual :: (Ord a) => [a] -> Bool
allLessThanOrEqual [] = True
allLessThanOrEqual [x] = True
allLessThanOrEqual (x:y:xs) = x <= y && allLessThanOrEqual (y:xs)

lessThanOrEqual' :: [Expr] -> Either Err Expr
lessThanOrEqual' [] = Left Err { reason = "Could not compare, list expression is empty" }
lessThanOrEqual' exprList = unsafeBoolCleanup $ allLessThanOrEqual exprList

defaultEnv = Env {
  data' = Data.Map.fromList [
    ("+", Func sum'),
    ("-", Func subtract'),
    ("=", Func equal'),
    (">", Func greaterThan'),
    ("<", Func lessThan'),
    (">=", Func greaterThanOrEqual'),
    ("<=", Func lessThanOrEqual')
                            ]
                  }

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

getElementByIndex :: Int -> [a] -> Maybe a
getElementByIndex idx list = case (safeHead $ filter (\(_, i) -> i == idx) (zip list [0..])) of
                               Just j -> Just (fst j)
                               Nothing -> Nothing

evalIfArgs :: Env -> [Expr] -> Either Err (Env, Expr)
evalIfArgs env argForms = case ((length argForms) /= 3) of
                            True -> Left Err { reason = "'if' should have a condition and two parameters, not less, not more" }
                            False -> case (eval env (head argForms)) of
                                       Left err -> Left err
                                       Right (newEnv, expr) -> case expr of
                                                       Boolean b -> let formIdx = if b then 1 else 2
                                                                     in case (getElementByIndex (formIdx) argForms) of
                                                                          Nothing -> Left Err { reason = "Expected form idx: " ++ show formIdx }
                                                                          Just resForm -> eval env resForm
                                                       _ -> Left Err { reason = "'if' result was not a boolean" }

addKeyToEnv :: String -> Expr -> Env -> Env
addKeyToEnv key expr env = Env { data' = insert key expr (data' env) }

evalDefArgs :: Env -> [Expr] -> Either Err (Env, Expr)
evalDefArgs env argForms = case ((length argForms) /= 2) of
                             True -> Left Err { reason = "'def' should have a name and a value, not less, not more" }
                             False -> case (head argForms) of
                                        Symbol s -> case (eval env (head $ tail argForms)) of
                                                      Left err -> Left err
                                                      Right (newEnv, expr) -> let envWithDef = addKeyToEnv (show $ head argForms) expr newEnv
                                                                               in Right (envWithDef, (head argForms))
                                        _ -> Left Err { reason = "'def' name should be a symbol (aka string)" }

evalBuiltInForm :: Env -> Expr -> [Expr] -> Maybe (Either Err (Env, Expr))
evalBuiltInForm env expr argForms = case expr of
                                      Symbol s
                                        | s == "if" -> Just (evalIfArgs env argForms)
                                        | s == "def" -> Just (evalDefArgs env argForms)
                                        | otherwise -> Nothing
                                      _ -> Nothing

callEvalOnArg :: Env -> Expr -> Either Err (Env, Expr)
callEvalOnArg env expr = eval env expr

extractExprFromTuple :: Either Err (Env, Expr) -> Either Err Expr
extractExprFromTuple either = case either of
                                Left err -> Left err
                                Right (env, expr) -> Right expr

createEnvExprTuple :: Env -> Either Err Expr -> Either Err (Env, Expr)
createEnvExprTuple env either = case either of
                                  Left err -> Left err
                                  Right expr -> Right (env, expr)

eval :: Env -> Expr -> Either Err (Env, Expr)
eval env expr = case expr of
                  Boolean b -> Right (env, expr)
                  Symbol s -> case Data.Map.lookup s (data' env) of
                                Nothing -> Left Err { reason = "Unexpected symbol '" ++ s ++ "'" }
                                Just v -> Right (env, v)
                  Number n -> Right (env, expr)
                  List list -> case (null list) of
                                 True -> Left Err { reason = "Expected a non empty list" }
                                 False -> case (evalBuiltInForm env (head list) (tail list)) of
                                            Just result -> result
                                            Nothing -> case (eval env (head list)) of
                                                         Left err -> Left err
                                                         Right (newEnv, expr) -> case expr of
                                                                         Func func -> let evalArgsTuple = (map (callEvalOnArg newEnv) (tail list))
                                                                                          evalArgs = (map extractExprFromTuple evalArgsTuple)
                                                                                       in case (any isLeft evalArgs) of
                                                                                            True -> Left (head (lefts evalArgs))
                                                                                            False -> (createEnvExprTuple newEnv (func (rights evalArgs)))
                                                                         _ -> Left Err { reason = "First form must be a function" }
                  Func _ -> Left Err { reason = "Unexpected form (func)" }
                  Lambda l -> Left Err { reason = "Unexpected form (lambda)" }


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

main = do
  repl defaultEnv

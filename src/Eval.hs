{-# LANGUAGE LambdaCase #-}

module Eval 
    ( eval
    ) where

import Data.Map (fromList)
import Data.Either (lefts, isLeft, rights)

import Expr (Expr(..), LambdaData(..))
import Err (Err(..))
import Env (Env(..), addKeyToEnv, getExprOfEnv)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

getElementByIndex :: Int -> [a] -> Maybe a
getElementByIndex idx list = case safeHead $ filter (\(_, i) -> i == idx) (zip list [0..]) of
                               Just j -> Just (fst j)
                               Nothing -> Nothing

evalIfArgs :: Env -> [Expr] -> Either Err (Env, Expr)
evalIfArgs env argForms = if length argForms /= 3
                            then Left Err { reason = "'if' should have a condition and two parameters, not less, not more" }
                            else case eval env (head argForms) of
                                       Left err -> Left err
                                       Right (newEnv, expr) -> case expr of
                                                       Boolean b -> let formIdx = if b then 1 else 2
                                                                     in case getElementByIndex formIdx argForms of
                                                                          Nothing -> Left Err { reason = "Expected form idx: " ++ show formIdx }
                                                                          Just resForm -> eval env resForm
                                                       _ -> Left Err { reason = "'if' result was not a boolean" }

evalDefArgs :: Env -> [Expr] -> Either Err (Env, Expr)
evalDefArgs env argForms = if length argForms /= 2
                             then Left Err { reason = "'def' should have a name and a value, not less, not more" }
                             else case head argForms of
                                        Symbol s -> case eval env (head $ tail argForms) of
                                                      Left err -> Left err
                                                      Right (newEnv, expr) -> let envWithDef = addKeyToEnv (show $ head argForms) expr newEnv
                                                                               in Right (envWithDef, head argForms)
                                        _ -> Left Err { reason = "'def' name should be a symbol (aka string)" }

evalLambdaArgs :: Env -> [Expr] -> Either Err (Env, Expr)
evalLambdaArgs env argForms = if length argForms /= 2
                                then Left Err { reason = "'fn' should have a parameters list and a body, not less, not more" }
                                else Right (env, Lambda LambdaData { params = head argForms, body = head $ tail argForms })

evalBuiltInForm :: Env -> Expr -> [Expr] -> Maybe (Either Err (Env, Expr))
evalBuiltInForm env expr argForms = case expr of
                                      Symbol s
                                        | s == "if" -> Just (evalIfArgs env argForms)
                                        | s == "def" -> Just (evalDefArgs env argForms)
                                        | s == "fn" -> Just (evalLambdaArgs env argForms)
                                        | otherwise -> Nothing
                                      _ -> Nothing

extractExprFromTuple :: Either Err (Env, Expr) -> Either Err Expr
extractExprFromTuple either = case either of
                                Left err -> Left err
                                Right (env, expr) -> Right expr

createEnvExprTuple :: Env -> Either Err Expr -> Either Err (Env, Expr)
createEnvExprTuple env either = case either of
                                  Left err -> Left err
                                  Right expr -> Right (env, expr)

parseListOfSymbolStrings :: Expr -> Either Err [String]
parseListOfSymbolStrings form = case form of
                                  List list -> let l = map (\case
                                                            Symbol s -> Right s
                                                            _ -> Left Err { reason = "Expected symbols in the arguments list" }) list
                                                in if any isLeft l
                                                     then Left (head (lefts l))
                                                     else Right (rights l)
                                  _ -> Left Err { reason = "Expected arguments to be a form" }

buildEnvForLambda :: Env -> Expr -> [Expr] -> Either Err Env
buildEnvForLambda outerEnv params argForms = case parseListOfSymbolStrings params of
                                               Left err -> Left err
                                               Right exprList -> if length exprList /= length argForms
                                                                   then Left Err { reason = "Expected " ++ show (length exprList) ++ " arguments, got " ++ show (length argForms) }
                                                                   else case evalForms outerEnv argForms of
                                                                              Left err -> Left err
                                                                              Right forms -> Right Env {
                                                                                              data' = Data.Map.fromList (zip exprList forms),
                                                                                              outer = Just outerEnv
                                                                                                       }

evalForms :: Env -> [Expr] -> Either Err [Expr]
evalForms env argForms = let evalArgsTuple = map (eval env) argForms
                             evalArgs = map extractExprFromTuple evalArgsTuple
                          in if any isLeft evalArgs
                               then Left (head (lefts evalArgs))
                               else Right (rights evalArgs)

{-# ANN eval "HLint: ignore Use fromMaybe" #-}
eval :: Env -> Expr -> Either Err (Env, Expr)
eval env expr = case expr of
                  Boolean b -> Right (env, expr)
                  Symbol s -> case getExprOfEnv s env of
                                Nothing -> Left Err { reason = "Unexpected symbol '" ++ s ++ "'" }
                                Just v -> Right (env, v)
                  Number n -> Right (env, expr)
                  List list -> if null list
                                 then Left Err { reason = "Expected a non empty list" }
                                 else case evalBuiltInForm env (head list) (tail list) of
                                            Just result -> result
                                            Nothing -> case eval env (head list) of
                                                         Left err -> Left err
                                                         Right (newEnv, expr) -> case expr of
                                                                         Func func -> case evalForms newEnv (tail list) of
                                                                                        Left err -> Left err
                                                                                        Right forms -> createEnvExprTuple newEnv (func forms)
                                                                         Lambda lambda -> case buildEnvForLambda newEnv (params lambda) (tail list) of
                                                                                            Left err -> Left err
                                                                                            Right envForLambda ->  eval envForLambda (body lambda)
                                                                         _ -> Left Err { reason = "First form must be a function" }
                  Func _ -> Left Err { reason = "Unexpected form (func)" }
                  Lambda l -> Left Err { reason = "Unexpected form (lambda)" }

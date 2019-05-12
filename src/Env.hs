module Env
    ( Env(..)
    , defaultEnv
    , addKeyToEnv
    , getExprOfEnv
    ) where

import Data.Map (Map, fromList, lookup, insert)
import qualified Control.Exception as Exc
import System.IO.Unsafe (unsafePerformIO)

import Expr (Expr(..))
import Err (Err(..))

data Env = Env {
  data' :: Map String Expr,
  outer :: Maybe Env
               }

isNumber :: Expr -> Bool
isNumber expr = case expr of
                   Number n -> True
                   _ -> False

sumAux :: Float -> Expr -> Float
sumAux acc expr = case expr of
                   Number n -> acc + n
                   _ -> error "Should not sum Expressions that are not Numbers"

sum' :: [Expr] -> Either Err Expr
sum' [] = Left Err { reason = "Could not sum, list expression is empty" }
sum' exprList = if any isNumber exprList
                   then Right (Number (foldl sumAux 0.0 exprList))
                   else Left Err { reason = "Could not sum, not all expressions in list are Numbers" }

getExprNumber :: Expr -> Float
getExprNumber expr = case expr of
                         Number n -> n
                         _ -> error "Could not get Number out of an Expression that is not a Number"

subtract' :: [Expr] -> Either Err Expr
subtract' [] = Left Err { reason = "Could not subtract, list expression is empty" }
subtract' (x:xs) = if any isNumber (x : xs)
                   then Right (Number (getExprNumber x - foldl sumAux 0.0 xs))
                   else Left Err { reason = "Could not subtract, not all expressions in list are Numbers" }

-- adapted from: https://stackoverflow.com/questions/6121256/efficiently-checking-that-all-the-elements-of-a-big-list-are-the-same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

-- copied from: https://stackoverflow.com/questions/27392547/return-the-first-line-of-a-string-in-haskell
firstLine :: String -> String
firstLine = head . lines

{-# ANN unsafeBoolCleanup "HLint: ignore Evaluate" #-}
-- adapted from: https://stackoverflow.com/questions/4243117/how-to-catch-and-ignore-a-call-to-the-error-function
unsafeBoolCleanup :: Bool -> Either Err Expr
unsafeBoolCleanup x = unsafePerformIO $ Exc.catch (x `seq` return (Right (Boolean x))) handler
    where
      handler exc = return (Left Err { reason = firstLine $ show exc }) `const`  (exc :: Exc.ErrorCall)

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
                            ],
  outer = Nothing
                  }

addKeyToEnv :: String -> Expr -> Env -> Env
addKeyToEnv key expr env = Env {
  data' = insert key expr (data' env),
  outer = outer env
                               }

getExprOfEnv :: String -> Env -> Maybe Expr
getExprOfEnv key env = case Data.Map.lookup key (data' env) of
                         Just v -> Just v
                         Nothing -> case outer env of
                                      Nothing -> Nothing
                                      Just o -> getExprOfEnv key o

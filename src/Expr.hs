module Expr
  ( Expr(..)
  , LambdaData(..)
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Text.Show.Functions

import Err (Err(..))

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

data LambdaData = LambdaData { params :: Expr, body :: Expr }

instance Show LambdaData where
  show l = "<lambda>"


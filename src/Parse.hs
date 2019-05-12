module Parse 
    ( parse
    ) where

import Text.Read (readMaybe)

import Expr (Expr(..))
import Err (Err(..))

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

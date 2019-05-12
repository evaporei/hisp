module Err 
    ( Err(..)
    ) where

newtype Err = Err { reason :: String }

instance Show Err where
  show e = "Error: " ++ reason e

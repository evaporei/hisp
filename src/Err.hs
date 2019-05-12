module Err 
    ( Err(..)
    ) where

data Err = Err { reason :: String }

instance Show Err where
  show e = "Error: " ++ (reason e)

module Crepitans.Solver (
  Solver(..)
  ) where

data Solver = CVC4
            | Yices
            | Z3
            deriving (Show)

{-# LANGUAGE TemplateHaskell #-}
module Crepitans.Panic (
    Component(..)
  , P.panic
  ) where

import qualified Panic as P

data Component = Loader
               | Evaluator
               | SymbolicExecution
  deriving (Show)

instance P.PanicComponent Component where
  panicComponentName = show
  panicComponentIssues _ = "https://github.com/travitch/crepitans/issues"
  panicComponentRevision = $(P.useGitRevision)

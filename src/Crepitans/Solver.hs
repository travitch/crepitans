{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Crepitans.Solver (
    Solver(..)
  , SomeOnlineSolver(..)
  , makeOnlineSolver
  ) where

import           Data.Bits ( (.|.) )
import qualified What4.Expr.Builder as WEB
import qualified What4.ProblemFeatures as WP
import qualified What4.Protocol.Online as WPO
import qualified What4.Protocol.SMTLib2 as SMT2
import qualified What4.Solver.CVC4 as CVC4
import qualified What4.Solver.Yices as Yices
import qualified What4.Solver.Z3 as Z3

import qualified Lang.Crucible.Backend.Online as LCBO

data Solver = CVC4
            | Yices
            | Z3
            deriving (Show)


-- | There are some options here, but these are good defaults. We can make this
-- configurable later if we want
defaultProblemFeatures :: WP.ProblemFeatures
defaultProblemFeatures =     WP.useBitvectors
                         .|. WP.useSymbolicArrays
                         .|. WP.useStructs
                         .|. WP.useLinearArithmetic

data SomeOnlineSolver scope st fs where
  SomeOnlineSolver :: (WPO.OnlineSolver solver) => LCBO.OnlineBackend solver scope st fs -> SomeOnlineSolver scope st fs

type CVC4 = SMT2.Writer CVC4.CVC4
type Yices = Yices.Connection
type Z3 = SMT2.Writer Z3.Z3

makeOnlineSolver
  :: WEB.ExprBuilder scope st fs
  -> Solver
  -> IO (SomeOnlineSolver scope st fs)
makeOnlineSolver sym solver =
  case solver of
    CVC4 -> SomeOnlineSolver <$> LCBO.newOnlineBackend @CVC4 sym defaultProblemFeatures
    Yices -> SomeOnlineSolver <$> LCBO.newOnlineBackend @Yices sym defaultProblemFeatures
    Z3 -> SomeOnlineSolver <$> LCBO.newOnlineBackend @Z3 sym defaultProblemFeatures

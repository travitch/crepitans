{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Fatal run-time exceptions that can occur while evaluating a script
--
-- Note that it is intended that there are very few of these and that most
-- errors will be recoverable in some way
module Crepitans.Exceptions (
  FatalException(..)
  ) where

import qualified Control.Exception as CE
import qualified Language.Scheme.Types as LST
import           Numeric.Natural ( Natural )
import qualified Prettyprinter as PP

import qualified Crepitans.ArgumentMapping as CA

data FatalException where
  LispParseException :: LST.LispError -> FatalException
  LispEvaluationError :: LST.LispError -> FatalException
  ArgumentMappingError :: CA.ArgumentTypeError -> FatalException
  ELFHeaderDecodeError :: Natural -> String -> FatalException

deriving instance Show FatalException
instance CE.Exception FatalException

ppFatalException :: FatalException -> PP.Doc ann
ppFatalException x =
  case x of
    LispParseException lx -> PP.pretty "Lisp parse error: " <> PP.viaShow lx
    LispEvaluationError lx -> PP.pretty "Lisp evaluation error: " <> PP.viaShow lx
    ArgumentMappingError ate -> PP.pretty ate
    ELFHeaderDecodeError off msg -> PP.pretty "Error decoding ELF header at offset " <> PP.pretty off <> PP.pretty ": " <> PP.pretty msg

instance PP.Pretty FatalException where
  pretty = ppFatalException

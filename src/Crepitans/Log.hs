{-# LANGUAGE GADTs #-}
module Crepitans.Log (
  LogMessage(..)
  ) where

import qualified Data.Text as DT
import qualified Prettyprinter as PP

import qualified Crepitans.Exceptions as CE

-- | Log events/messages that can be emitted by the system
--
-- The intent is that all output should be pushed through this interface to
-- enable streaming diagnostics and script output to be routed as-needed by the
-- caller.
data LogMessage where
  CodeDiscoveryDiagnostic :: DT.Text -> LogMessage
  UnsupportedFileType :: FilePath -> LogMessage
  FatalException :: CE.FatalException -> LogMessage

ppLogMessage :: LogMessage -> PP.Doc ann
ppLogMessage lm =
  case lm of
    CodeDiscoveryDiagnostic msg -> PP.pretty "Error during code discovery: " <> PP.pretty msg
    UnsupportedFileType p -> PP.pretty "Unsupported file type: " <> PP.pretty p
    FatalException x -> PP.pretty "Fatal exception: " <> PP.pretty x

instance PP.Pretty LogMessage where
  pretty = ppLogMessage

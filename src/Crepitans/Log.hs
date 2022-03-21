{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Crepitans.Log (
  LogMessage(..)
  ) where

import qualified Data.Text as DT
import qualified Prettyprinter as PP

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as DMD

import qualified Crepitans.Architecture as CA
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
  DiscoveryEvent :: (MC.MemWidth (MC.ArchAddrWidth arch)) => CA.ArchRepr arch -> DMD.DiscoveryEvent arch -> LogMessage

ppDiscoveryEvent :: (MC.MemWidth (MC.ArchAddrWidth arch)) => CA.ArchRepr arch -> DMD.DiscoveryEvent arch -> PP.Doc ann
ppDiscoveryEvent _repr evt =
  case evt of
    DMD.ReportAnalyzeFunction addr -> PP.pretty "Code discovery analyzing a function at address " <> PP.viaShow addr
    DMD.ReportAnalyzeFunctionDone dfi -> PP.pretty "Code discovery finished analyzing a function at address " <> PP.viaShow (DMD.discoveredFunAddr dfi)
    DMD.ReportIdentifyFunction src tgt rsn ->
      PP.hcat [ PP.pretty "Code discovery identified a candidate entry point at " <> PP.viaShow tgt
              , PP.pretty " while analyzing a function at " <> PP.viaShow src
              , PP.pretty " because " <> PP.viaShow rsn
              ]
    DMD.ReportAnalyzeBlock faddr baddr ->
      PP.hcat [ PP.pretty "Code discovery identified a block at " <> PP.viaShow baddr
              , PP.pretty " while analyzing a function at " <> PP.viaShow faddr
              ]

ppLogMessage :: LogMessage -> PP.Doc ann
ppLogMessage lm =
  case lm of
    CodeDiscoveryDiagnostic msg -> PP.pretty "Error during code discovery: " <> PP.pretty msg
    UnsupportedFileType p -> PP.pretty "Unsupported file type: " <> PP.pretty p
    FatalException x -> PP.pretty "Fatal exception: " <> PP.pretty x
    DiscoveryEvent repr evt -> ppDiscoveryEvent repr evt

instance PP.Pretty LogMessage where
  pretty = ppLogMessage

module Crepitans (
  runAnalysis
  ) where

import qualified Control.Exception as X
import qualified Data.Text as DT
import qualified Lumberjack as LJ

import qualified Crepitans.Log as CL
import qualified Crepitans.Panic as CP
import qualified Crepitans.Script.Fingerprint as SCF
import qualified Crepitans.Script.Scheme as CSS

-- | Wrap a catchable exception into a log for the logger, then return
wrapCatchable
  :: (X.Exception a)
  => LJ.LogAction IO CL.LogMessage
  -> (a -> CL.LogMessage)
  -> X.Handler ()
wrapCatchable logAction wrapper =
  X.Handler $ \x -> LJ.writeLog logAction (wrapper x)

-- | Turn any catchable exceptions into a log message and return nothing
catchingLoggableExceptions
  :: LJ.LogAction IO CL.LogMessage
  -> IO ()
  -> IO ()
catchingLoggableExceptions logAction a =
  X.catches a [ wrapCatchable logAction CL.FatalException
              ]

runAnalysis
  :: LJ.LogAction IO CL.LogMessage
  -- ^ The logger to use; note that essentially all program output is emitted through this structured logging interface
  -> FilePath
  -- ^ The name of the input file for both diagnostic purposes and for attempting to use the extension to choose the interpreter
  -> DT.Text
  -- ^ The contents of the script to execute
  -> IO ()
runAnalysis logAction filePath scriptContents = catchingLoggableExceptions logAction $
  case SCF.fingerprint filePath scriptContents of
    Just SCF.Scheme -> CSS.run logAction filePath scriptContents
    Just SCF.Python -> CP.panic CP.Loader "runAnalysis" ["Python scripts are not supported"]
    Just SCF.CodeQL -> CP.panic CP.Loader "runAnalysis" ["CodeQL scripts are not supported"]
    Nothing -> LJ.writeLog logAction (CL.UnsupportedFileType filePath)

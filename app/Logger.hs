module Logger (
    Logger
  , logger
  , makeLogger
  , finishLogger
  ) where

import qualified Control.Concurrent.Async as CCA
import qualified Control.Concurrent.Chan as CCC
import qualified Lumberjack as LJ
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PRT
import qualified System.IO as SI

import qualified Crepitans.Log as CL

import qualified Options as O

-- | A wrapper around all of the logger state to enable it to be cleanly shut down
data Logger =
  Logger { logger :: LJ.LogAction IO CL.LogMessage
         -- ^ The user-facing 'LJ.LogAction'; we cache the actual object
         -- (instead of making one on-demand) to preserve sharing
         , loggerChannel :: CCC.Chan (Maybe CL.LogMessage)
         -- ^ The underlying channel backing the logger; it takes a 'Maybe'
         -- value so that passing 'Nothing' shuts down the logger
         , loggerConsumer :: CCA.Async ()
         -- ^ The main logger thread
         }

consumeLogs
  :: CCC.Chan (Maybe CL.LogMessage)
  -> IO ()
consumeLogs c = do
  mmsg <- CCC.readChan c
  case mmsg of
    Nothing -> return ()
    Just msg -> do
      PRT.hPutDoc SI.stdout (PP.pretty msg <> PP.line)
      consumeLogs c

-- | A logger to record all of the output from the analysis
--
-- This is a high-level process that will split output into multiple output
-- streams (e.g., a diagnostics file vs stdout)
makeLogger
  :: O.Options
  -- ^ User options so that we can pick out the ones necessary for setting up log destinations
  -> IO Logger
makeLogger _opts = do
  msgChan <- CCC.newChan
  consumer <- CCA.async (consumeLogs msgChan)
  return Logger { logger = LJ.LogAction $ \msg -> CCC.writeChan msgChan (Just msg)
                , loggerChannel = msgChan
                , loggerConsumer = consumer
                }


finishLogger :: Logger -> IO ()
finishLogger l = do
  CCC.writeChan (loggerChannel l) Nothing
  CCA.wait (loggerConsumer l)

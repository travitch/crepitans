{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Crepitans.Library.Scripting (
    loadBinary
  , formatBinaryHeader
  , discoverFunctions
  ) where

import           Data.Parameterized.Context ( EmptyCtx, type (::>), pattern Empty, pattern (:>) )
import qualified Data.Parameterized.Context as Ctx
import qualified Lumberjack as LJ

import qualified Crepitans.ArgumentMapping as CA
import qualified Crepitans.Library as CL
import qualified Crepitans.Log as CLog

loadBinary
  :: Ctx.Assignment CA.Argument (EmptyCtx ::> CA.PathK)
  -> IO (CA.Argument CA.BinaryK)
loadBinary (Empty :> CA.Path path) = CA.Binary <$> CL.loadBinary path

formatBinaryHeader
  :: Ctx.Assignment CA.Argument (EmptyCtx ::> CA.BinaryK)
  -> IO (CA.Argument CA.StringK)
formatBinaryHeader (Empty :> CA.Binary bin) = CA.String_ <$> CL.formatBinaryHeader bin

discoverFunctions
  :: LJ.LogAction IO CLog.LogMessage
  -> Ctx.Assignment CA.Argument (EmptyCtx ::> CA.BinaryK)
  -> IO (CA.Argument CA.DiscoveryK)
discoverFunctions logAction (Empty :> CA.Binary bin) = CA.DiscoveryInfo <$> CL.discoverFunctions logAction bin

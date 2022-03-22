{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Crepitans.Library.Scripting (
    loadBinary
  , formatBinaryHeader
  , discoverFunctions
  , discoveredFunctions
  , functionAddress
  , functionName
  ) where

import qualified Data.Parameterized.Classes as DPC
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

discoveredFunctions
  :: Ctx.Assignment CA.Argument (EmptyCtx ::> CA.DiscoveryK)
  -> IO (CA.Argument (CA.VectorK CA.FunctionK))
discoveredFunctions (Empty :> CA.DiscoveryInfo info) =
  (CA.Vector_ DPC.knownRepr . fmap CA.Function) <$> CL.discoveredFunctions info

functionAddress
  :: Ctx.Assignment CA.Argument (EmptyCtx ::> CA.FunctionK)
  -> IO (CA.Argument CA.AddressK)
functionAddress (Empty :> CA.Function f) = CA.Address <$> CL.functionAddress f

functionName
  :: Ctx.Assignment CA.Argument (EmptyCtx ::> CA.FunctionK)
  -> IO (CA.Argument CA.StringK)
functionName (Empty :> CA.Function f) = CA.String_ <$> CL.functionName f

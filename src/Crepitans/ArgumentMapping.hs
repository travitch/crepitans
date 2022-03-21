{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Data types and utilities for mapping arguments from Haskell to scripting
-- languages (and back)
module Crepitans.ArgumentMapping (
  -- * Value types
    Binary(..)
  , DiscoveryInfo(..)
  -- * Scripting language wrappers
  , ArgumentK
  , BinaryK
  , DiscoveryK
  , PathK
  , StringK
  , Argument(..)
  , ArgumentRepr(..)
  , ArgumentMapping(..)
  , ArgumentTypeError(..)
  , mapToHaskell
  ) where

import           Control.Monad.Trans ( lift )
import qualified Control.Monad.Except as CME
import qualified Data.ElfEdit as DE
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Vector as DV
import qualified Prettyprinter as PP

import qualified Data.Macaw.BinaryLoader as DMB
import qualified Data.Macaw.Discovery as DMD

import qualified Crepitans.Architecture as CA
import qualified Crepitans.Panic as CP

-- | A wrapper around any of the binary formats supported by the library
--
-- These are *raw* binary formats with minimal parsing and no code discovery
data Binary where
  ELFBinary :: DE.ElfHeaderInfo w -> Binary

-- | The results of running code discovery
--
-- This contains an arch repr to enable recovering instances, as well as a
-- reference to the original binary (which can be useful in case multiple
-- binaries are being examined concurrently)
data DiscoveryInfo where
  DiscoveryInfoWith :: (DMB.BinaryLoader arch binFmt) => Binary -> DMB.LoadedBinary arch binFmt -> CA.ArchRepr arch -> DMD.DiscoveryState arch -> DiscoveryInfo

-- | Type-level GADT tags for the 'Argument' type
data ArgumentK = BinaryK
               | PathK
               | StringK
               | DiscoveryK

type BinaryK = 'BinaryK
type PathK = 'PathK
type StringK = 'StringK
type DiscoveryK = 'DiscoveryK

data ArgumentRepr tp where
  BinaryRepr :: ArgumentRepr BinaryK
  DiscoveryRepr :: ArgumentRepr DiscoveryK
  PathRepr :: ArgumentRepr PathK
  StringRepr :: ArgumentRepr StringK

deriving instance Show (ArgumentRepr tp)

data Argument (tp :: ArgumentK) where
  Binary :: Binary -> Argument BinaryK
  DiscoveryInfo :: DiscoveryInfo -> Argument DiscoveryK
  Path :: FilePath -> Argument PathK
  String_ :: String -> Argument StringK

-- | Per-language functions for mapping values between Haskell and scripting languages
--
-- This is data (rather than a class) so that it can capture run-time values
-- (e.g., an environment) if needed.
--
-- The type parameter is the type of run-time values for the language
data ArgumentMapping m a =
  ArgumentMapping { toHaskell :: forall tp . a -> ArgumentRepr tp -> m (Maybe (Argument tp))
                  , fromHaskell :: forall tp . Argument tp -> a
                  , valueTypeName :: a -> String
                  }

-- | An error that arose when attempting to match a dynamic value against an expected Haskell type
data ArgumentTypeError where
  -- | The first 'String' is the name of the function being called (if available)
  --
  -- The 'Int' is the 0-based index into the argument list where the error occurred
  --
  -- The 'ArgumentRepr' is the expected type
  --
  -- The last 'String' is the actual type
  ArgumentTypeError :: String -> Int -> ArgumentRepr tp -> String -> ArgumentTypeError
  -- | The expected argument list length, the actual argument list length
  ArgumentListLengthMismatch :: String -> Int -> Int -> ArgumentTypeError

applyMapping
  :: (Monad m)
  => ArgumentMapping m a
  -> String
  -> DV.Vector a
  -> Ctx.Index tps tp
  -> ArgumentRepr tp
  -> CME.ExceptT ArgumentTypeError m (Argument tp)
applyMapping mapping context actuals idx expectedRepr
  | Just actual <- actuals DV.!? Ctx.indexVal idx = do
      mv <- lift $ toHaskell mapping actual expectedRepr
      case mv of
        Just v -> return v
        Nothing -> CME.throwError (ArgumentTypeError context (Ctx.indexVal idx) expectedRepr (valueTypeName mapping actual))
  | otherwise = CP.panic CP.Evaluator "applyMapping" ["Argument list length mismatch should have been caught in `mapToHaskell`"]

mapToHaskell
  :: (Monad m)
  => ArgumentMapping m a
  -> String
  -> DV.Vector a
  -> Ctx.Assignment ArgumentRepr tps
  -> m (Either ArgumentTypeError (Ctx.Assignment Argument tps))
mapToHaskell mapping context actuals expectedTypes
  | actualsLength /= expectedsLength = return (Left (ArgumentListLengthMismatch context expectedsLength actualsLength))
  | otherwise = CME.runExceptT $ Ctx.traverseWithIndex (applyMapping mapping context actuals) expectedTypes
  where
    actualsLength = DV.length actuals
    expectedsLength = Ctx.sizeInt (Ctx.size expectedTypes)



ppArgumentTypeError :: ArgumentTypeError -> PP.Doc ann
ppArgumentTypeError ate =
  case ate of
    ArgumentTypeError context index expectedRepr actualType ->
      PP.hcat [ PP.pretty "Type error in call to "
              , PP.pretty context
              , PP.pretty " where the argument at index "
              , PP.pretty index
              , PP.pretty " has type " <> PP.pretty actualType
              , PP.pretty " where it was expected to have type " <> PP.viaShow expectedRepr
              ]
    ArgumentListLengthMismatch context expectedLength actualLength ->
      PP.hcat [ PP.pretty "A call to function " <> PP.pretty context
              , PP.pretty " had " <> PP.pretty actualLength <> PP.pretty " arguments where "
              , PP.pretty expectedLength <> PP.pretty " were expected"
              ]

instance PP.Pretty ArgumentTypeError where
  pretty = ppArgumentTypeError

deriving instance Show ArgumentTypeError

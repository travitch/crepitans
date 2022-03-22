{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Data types and utilities for mapping arguments from Haskell to scripting
-- languages (and back)
module Crepitans.ArgumentMapping (
  -- * Value types
    Binary(..)
  , DiscoveryInfo(..)
  , DiscoveryInfoWith(..)
  , Function(..)
  , Address(..)
  -- * Scripting language wrappers
  , ArgumentK
  , BinaryK
  , DiscoveryK
  , PathK
  , AddressK
  , StringK
  , FunctionK
  , VectorK
  , Argument(..)
  , ArgumentRepr(..)
  , ArgumentMapping(..)
  , ArgumentTypeError(..)
  , mapToHaskell
  ) where

import qualified Control.Monad.Except as CME
import           Control.Monad.Trans ( lift )
import qualified Data.ElfEdit as DE
import qualified Data.Parameterized.Classes as DPC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Vector as DV
import           Data.Word ( Word64 )
import qualified Prettyprinter as PP

import qualified Data.Macaw.BinaryLoader as DMB
import qualified Data.Macaw.CFG.Core as DMC
import qualified Data.Macaw.Discovery as DMD
import qualified Data.Macaw.Memory as DMM

import qualified Crepitans.Architecture as CA
import qualified Crepitans.Panic as CP

-- | A wrapper around any of the binary formats supported by the library
--
-- These are *raw* binary formats with minimal parsing and no code discovery
data Binary where
  ELFBinary :: DE.ElfHeaderInfo w -> Binary

data DiscoveryInfoWith arch where
  DiscoveryInfoWith_ :: (DMB.BinaryLoader arch binFmt, DMM.MemWidth (DMC.ArchAddrWidth arch))
                     => Binary
                     -> DMB.LoadedBinary arch binFmt
                     -> CA.ArchRepr arch
                     -> DMD.DiscoveryState arch
                     -> DiscoveryInfoWith arch

-- | The results of running code discovery
--
-- This contains an arch repr to enable recovering instances, as well as a
-- reference to the original binary (which can be useful in case multiple
-- binaries are being examined concurrently)
data DiscoveryInfo where
  DiscoveryInfoWith :: DiscoveryInfoWith arch -> DiscoveryInfo

-- | A wrapper around machine functions, along with the metadata necessary to do
-- additional analysis later
data Function where
  FunctionWith :: DiscoveryInfoWith arch -> DMD.DiscoveryFunInfo arch ids -> Function

-- | Different forms of address that a user can pass around
--
-- All library functions will have to be prepared to deal with addresses in any
-- of these forms (or throw errors if a certain form is not possible to handle
-- in some context). The goal here is that users need to be able to specify
-- addresses as numeric literals, while the library must be able to convert them
-- to appropriate internal values where appropriate. Those internal forms might
-- be one of (depending on context):
--
--  * MemSegmentOff
--  * LLVMPointer
--  * MemAddr
--
-- User addresses may be internalized to a normalized form when returned to the
-- scripting language
--
-- Note that this plethora of representations means that we will need custom
-- address comparison and arithmetic operations
data Address where
  -- | An address specified by the user as a bare literal (likely a hex
  -- literal). It will always be interpreted as the bit pattern associated with
  -- a bitvector of pointer-width bits.
  --
  -- Note that we don't know the pointer width until inside of the library, so
  -- errors may be deferred.
  UserSpecifiedAddress :: Word64 -> Address
  -- | A memory segment offset, which is the address form used inside of macaw
  SegmentOffset :: CA.ArchRepr arch -> DMC.ArchSegmentOff arch -> Address

-- | Type-level GADT tags for the 'Argument' type
data ArgumentK = BinaryK
               | PathK
               | StringK
               | DiscoveryK
               | FunctionK
               | AddressK
               | VectorK ArgumentK

type BinaryK = 'BinaryK
type PathK = 'PathK
type StringK = 'StringK
type DiscoveryK = 'DiscoveryK
type FunctionK = 'FunctionK
type AddressK = 'AddressK
type VectorK a = 'VectorK a

-- | Run-time representatives of all of the values that can be shared between Haskell and Scheme
--
-- These are used in the signatures of functions and for type testing when
-- calling Haskell from Scheme
data ArgumentRepr tp where
  BinaryRepr :: ArgumentRepr BinaryK
  DiscoveryRepr :: ArgumentRepr DiscoveryK
  FunctionRepr :: ArgumentRepr FunctionK
  PathRepr :: ArgumentRepr PathK
  StringRepr :: ArgumentRepr StringK
  AddressRepr :: ArgumentRepr AddressK
  VectorRepr :: ArgumentRepr tp' -> ArgumentRepr (VectorK tp')

deriving instance Show (ArgumentRepr tp)

instance DPC.KnownRepr ArgumentRepr BinaryK where knownRepr = BinaryRepr
instance DPC.KnownRepr ArgumentRepr DiscoveryK where knownRepr = DiscoveryRepr
instance DPC.KnownRepr ArgumentRepr PathK where knownRepr = PathRepr
instance DPC.KnownRepr ArgumentRepr StringK where knownRepr = StringRepr
instance DPC.KnownRepr ArgumentRepr FunctionK where knownRepr = FunctionRepr
instance DPC.KnownRepr ArgumentRepr AddressK where knownRepr = AddressRepr

-- | Wrappers for value types that are shared between Haskell and Scheme
--
-- These are a bit more coarse-grained than the Haskell APIs that are being
-- exposed to make use easier. Where necessary, these values maintain extra
-- run-time representative values to enable type recovery by pattern matching.
-- Note that these are mostly just wrappers at the language interface; the
-- Haskell functions remove these wrappers before operating on terms. At least
-- the Scheme frontend does not use them directly; it wraps the underlying
-- values in Dynamic values.
data Argument (tp :: ArgumentK) where
  Binary :: Binary -> Argument BinaryK
  DiscoveryInfo :: DiscoveryInfo -> Argument DiscoveryK
  Function :: Function -> Argument FunctionK
  Path :: FilePath -> Argument PathK
  Address :: Address -> Argument AddressK
  String_ :: String -> Argument StringK
  Vector_ :: ArgumentRepr tp' -> DV.Vector (Argument tp') -> Argument (VectorK tp')

-- | Per-language functions for mapping values between Haskell and scripting languages
--
-- This is data (rather than a class) so that it can capture run-time values
-- (e.g., an environment) if needed.
--
-- The type parameter is the type of run-time values for the language
--
-- Note that the Monad is a parameter (m) so that it can be pure or in IO, as
-- needed.
data ArgumentMapping m a =
  ArgumentMapping { toHaskell :: forall tp . ArgumentRepr tp -> a -> m (Maybe (Argument tp))
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
      mv <- lift $ toHaskell mapping expectedRepr actual
      case mv of
        Just v -> return v
        Nothing -> CME.throwError (ArgumentTypeError context (Ctx.indexVal idx) expectedRepr (valueTypeName mapping actual))
  | otherwise = CP.panic CP.Evaluator "applyMapping" ["Argument list length mismatch should have been caught in `mapToHaskell`"]

-- | Given run-time dynamic values (of type @a@) and a 'Ctx.Assignment' of
-- expected types, do run-time type checking while translating into Haskell
-- values. This can result in an 'ArgumentTypeError' if the types are not
-- correct.
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

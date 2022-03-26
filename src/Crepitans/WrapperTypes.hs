{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Wrapper types for the scripting interface
--
-- These typically quantify away type parameters, but retain enough information
-- to effectively use them. Together, these support a simpler interface for
-- macaw and crucible.
module Crepitans.WrapperTypes (
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
  -- * Run-time representatives
  , ArgumentRepr(..)
  ) where

import qualified Data.ElfEdit as DE
import qualified Data.Parameterized.Classes as DPC
import           Data.Word ( Word64 )

import qualified Data.Macaw.BinaryLoader as DMB
import qualified Data.Macaw.CFG.Core as DMC
import qualified Data.Macaw.Discovery as DMD
import qualified Data.Macaw.Memory as DMM

import qualified Crepitans.Architecture as CA

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

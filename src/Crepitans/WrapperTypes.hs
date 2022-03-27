{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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
  , discoveryInfoArchRepr
  , Function(..)
  , Address(..)
  -- ** Symbolic Execution
  , SymbolicExecutionContext(..)
  , executionFeatures
  , solver
  , BinarySymbolicExecutionContextWith(..)
  , initialRegisters
  , SymbolicExecutionConfig(..)
  , SymbolicExecutionResult_(..)
  , SymbolicExecutionResult(..)
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

import qualified Control.Lens.TH as CLT
import qualified Data.ElfEdit as DE
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Classes as DPC
import           Data.Word ( Word64 )
import           GHC.TypeLits ( type (<=) )
import qualified What4.Expr.Builder as WEB

import qualified Data.Macaw.BinaryLoader as DMB
import qualified Data.Macaw.CFG.Core as DMC
import qualified Data.Macaw.Discovery as DMD
import qualified Data.Macaw.Symbolic as DMS
import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.Simulator as LCS
import qualified Lang.Crucible.Types as LCT

import qualified Crepitans.Architecture as CA
import qualified Crepitans.Solver as CS

type ArchConstraints arch w =
  ( w ~ DMC.ArchAddrWidth arch
  , 16 <= w
  , DMS.SymArchConstraints arch
  )

-- | A wrapper around any of the binary formats supported by the library
--
-- These are *raw* binary formats with minimal parsing and no code discovery
data Binary where
  ELFBinary :: DE.ElfHeaderInfo w -> Binary

data DiscoveryInfoWith arch where
  DiscoveryInfoWith_ :: (DMB.BinaryLoader arch binFmt, ArchConstraints arch w)
                     => Binary
                     -> DMB.LoadedBinary arch binFmt
                     -> CA.ArchRepr arch
                     -> DMD.DiscoveryState arch
                     -> DiscoveryInfoWith arch

discoveryInfoArchRepr
  :: DiscoveryInfoWith arch
  -> CA.ArchRepr arch
discoveryInfoArchRepr (DiscoveryInfoWith_ _ _ rep _) = rep

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
  BinaryFunctionWith :: (ArchConstraints arch w) => DiscoveryInfoWith arch -> DMD.DiscoveryFunInfo arch ids -> Function

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

-- | Symbolic execution configuration that is independent of the backend
data SymbolicExecutionConfig p sym =
  SymbolicExecutionConfig { _executionFeatures :: [LCS.GenericExecutionFeature sym]
                          -- ^ Execution features to install when starting crucible
                          , _solver :: CS.Solver
                          -- ^ The solver to use for both path satisfiability
                          -- and goal solving (if any)
                          }

$(CLT.makeLenses ''SymbolicExecutionConfig)

-- | There are no lenses for the fields that are intended to be immutable
data BinarySymbolicExecutionContextWith sym arch =
  BinarySymEx { binaryDiscoveryInfo :: DiscoveryInfoWith arch
              , binaryDiscoveryFunInfo :: Some (DMD.DiscoveryFunInfo arch)
              , binaryArchVals :: DMS.ArchVals arch
              , binaryArchRepr :: CA.ArchRepr arch
              , _initialRegisters :: LCS.RegEntry sym (LCT.StructType (DMS.CtxToCrucibleType (DMS.ArchRegContext arch)))
              }

$(CLT.makeLenses ''BinarySymbolicExecutionContextWith)

-- | All of the configuration options and initial state required to start
-- symbolically executing a function.
--
-- This will be constructed from a 'Function' and have sufficient defaults to
-- start symbolic execution immediately if desired. It will also expose enough
-- knobs to customize symbolic execution as-needed (via a functional API)
data SymbolicExecutionContext where
  BinarySymbolicExecutionContext :: ( LCB.IsSymInterface sym
                                    , sym ~ WEB.ExprBuilder t st fs
                                    , ArchConstraints arch w
                                    )
                                 => sym
                                 -> SymbolicExecutionConfig p sym
                                 -> BinarySymbolicExecutionContextWith sym arch
                                 -> SymbolicExecutionContext


data SymbolicExecutionResult_ p sym ext arch tp =
  SymbolicExecutionResult_ { result :: LCS.ExecResult p sym ext (LCS.RegEntry sym tp)
                           , resultRepr :: LCT.TypeRepr tp
                           }

data SymbolicExecutionResult where
  SymbolicExecutionResult :: (LCB.IsSymBackend sym bak)
                          => bak
                          -> SymbolicExecutionResult_ p sym ext arch tp
                          -> SymbolicExecutionResult

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

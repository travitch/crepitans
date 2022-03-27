{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Crepitans.Library.SymbolicExecution (
    symbolicallyExecute
  , makeSymbolicExecutionContext
  ) where

import           Control.Lens ( (^.) )
import           Data.Bits ( (.|.) )
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Nonce as DPN
import           Data.String ( fromString )
import qualified Data.Text as DT
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified System.IO as IO
import qualified What4.Expr.Builder as WEB
import qualified What4.FunctionName as WF
import qualified What4.ProblemFeatures as WP
import qualified What4.ProgramLoc as WPL
import qualified What4.Protocol.Online as WPO
import qualified What4.Protocol.SMTLib2 as SMT2
import qualified What4.Solver.CVC4 as CVC4
import qualified What4.Solver.Yices as Yices
import qualified What4.Solver.Z3 as Z3

import qualified Data.Macaw.Architecture.Info as DMAI
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as DMC
import qualified Data.Macaw.Discovery as DMD
import qualified Data.Macaw.Memory as DMM
import qualified Data.Macaw.Symbolic as DMS
import qualified Data.Macaw.Symbolic.Memory as DMSM
import           Data.Macaw.X86.Symbolic ()
import           Data.Macaw.PPC.Symbolic ()
import           Data.Macaw.AArch32.Symbolic ()
import qualified Lang.Crucible.Backend as LCB
import qualified Lang.Crucible.Backend.Online as LCBO
import qualified Lang.Crucible.CFG.Core as LCCC
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Lang.Crucible.LLVM.DataLayout as LCLD
import qualified Lang.Crucible.LLVM.Intrinsics as LCLI
import qualified Lang.Crucible.LLVM.MemModel as LCLM
import qualified Lang.Crucible.Simulator as LCS
import qualified Lang.Crucible.Simulator.GlobalState as LCSG
import qualified Lang.Crucible.Types as LCT

import qualified Crepitans.Architecture as CA
import qualified Crepitans.Panic as CP
import qualified Crepitans.Solver as CS
import qualified Crepitans.WrapperTypes as CW

-- | Create a crucible-friendly name for a macaw function
--
-- If there is no symbol associated with the function, use its address as its
-- name.
discoveryFunName
  :: (DMM.MemWidth (DMC.ArchAddrWidth arch))
  => DMD.DiscoveryFunInfo arch ids
  -> WF.FunctionName
discoveryFunName dfi =
  WF.functionNameFromText txt
  where
    txt = TE.decodeUtf8With TEE.lenientDecode (DMD.discoveredFunName dfi)

-- | Construct a Crucible CFG for a macaw function
toCrucibleCFG
  :: (DMM.MemWidth (DMC.ArchAddrWidth arch))
  => DMS.MacawSymbolicArchFunctions arch
  -> DMD.DiscoveryFunInfo arch ids
  -> IO (LCCC.SomeCFG (DMS.MacawExt arch)
                      (Ctx.EmptyCtx Ctx.::> LCT.StructType (DMS.CtxToCrucibleType (DMS.ArchRegContext arch)))
                      (LCT.StructType (DMS.CtxToCrucibleType (DMS.ArchRegContext arch))))
toCrucibleCFG archFns dfi = do
  halloc <- CFH.newHandleAllocator
  let fnName = discoveryFunName dfi
  let posFn = WPL.OtherPos . fromString . show
  DMS.mkFunCFG archFns halloc fnName posFn dfi

-- | There are some options here, but these are good defaults. We can make this
-- configurable later if we want
defaultProblemFeatures :: WP.ProblemFeatures
defaultProblemFeatures =     WP.useBitvectors
                         .|. WP.useSymbolicArrays
                         .|. WP.useStructs
                         .|. WP.useLinearArithmetic

data SomeOnlineSolver scope st fs where
  SomeOnlineSolver :: (WPO.OnlineSolver solver) => LCBO.OnlineBackend solver scope st fs -> SomeOnlineSolver scope st fs

type CVC4 = SMT2.Writer CVC4.CVC4
type Yices = Yices.Connection
type Z3 = SMT2.Writer Z3.Z3

makeOnlineSolver
  :: WEB.ExprBuilder scope st fs
  -> CS.Solver
  -> IO (SomeOnlineSolver scope st fs)
makeOnlineSolver sym solver =
  case solver of
    CS.CVC4 -> SomeOnlineSolver <$> LCBO.newOnlineBackend @CVC4 sym defaultProblemFeatures
    CS.Yices -> SomeOnlineSolver <$> LCBO.newOnlineBackend @Yices sym defaultProblemFeatures
    CS.Z3 ->SomeOnlineSolver <$> LCBO.newOnlineBackend @Z3 sym defaultProblemFeatures

-- | We don't want to generate any extra pointer validity assertions since we
-- are supporting exploration rather than verification
noPointerValidityPred :: DMS.MkGlobalPointerValidityAssertion sym w
noPointerValidityPred _ _ _ _ = return Nothing

lookupFunctionHandle
  :: DMS.LookupFunctionHandle p sym arch
lookupFunctionHandle = DMS.unsupportedFunctionCalls "crepitans"

lookupSystemCall
  :: DMS.LookupSyscallHandle p sym arch
lookupSystemCall = DMS.unsupportedSyscalls "crepitans"

-- | Crucible extension for evaluating machine code
--
-- FIXME: It currently uses the default macaw implementation. We need to
-- override many of the operations to be more permissive
machineCodeExtension
  :: ( w ~ (DMC.ArchAddrWidth arch)
     , LCB.IsSymInterface sym
     , LCLM.HasLLVMAnn sym
     , ?memOpts :: LCLM.MemOptions
     )
  => DMS.MacawArchEvalFn p sym LCLM.Mem arch
  -> LCS.GlobalVar LCLM.Mem
  -> DMS.GlobalMap sym LCLM.Mem w
  -> LCS.ExtensionImpl p sym (DMS.MacawExt arch)
machineCodeExtension archEvalFns memVar globalMap =
  DMS.macawExtensions archEvalFns memVar globalMap lookupFunctionHandle lookupSystemCall noPointerValidityPred

data EmptyState t = EmptyState

makeSymbolicExecutionContext
  :: CW.Function
  -> IO CW.SymbolicExecutionContext
makeSymbolicExecutionContext (CW.BinaryFunctionWith discInfo dfi) = do
  sym <- WEB.newExprBuilder WEB.FloatRealRepr EmptyState DPN.globalNonceGenerator
  let symExConf = CW.SymbolicExecutionConfig { CW._executionFeatures = []
                                             , CW._solver = CS.Yices
                                             }
  let archVals =
        case CW.discoveryInfoArchRepr discInfo of
          CA.X86_64
            | Just av <- DMS.archVals CA.X86_64 Nothing -> av
          CA.PPC32
            | Just av <- DMS.archVals CA.PPC32 Nothing -> av
          CA.PPC64
            | Just av <- DMS.archVals CA.PPC64 Nothing -> av
          CA.AArch32
            | Just av <- DMS.archVals CA.AArch32 Nothing -> av
          -- This case is a panic because we shouldn't have any options by which the user could select an unsupported architecture.
          --
          -- Note that the 'DMS.archVals' function really should be total
          repr -> CP.panic CP.SymbolicExecution "makeSymbolicExecutionContext" ["Missing symbolic implementation for architecture " ++ show repr]

  let binSymEx = CW.BinarySymEx { CW.binaryDiscoveryInfo = discInfo
                                , CW.binaryDiscoveryFunInfo = Some dfi
                                , CW.binaryArchVals = archVals
                                , CW.binaryArchRepr = CW.discoveryInfoArchRepr discInfo
                                , CW._initialRegisters = error "FIXME: Populate initial registers"
                                }
  return (CW.BinarySymbolicExecutionContext sym symExConf binSymEx)

endianness
  :: DMD.DiscoveryState arch
  -> LCLD.EndianForm
endianness ds =
  case DMAI.archEndianness (DMD.archInfo ds) of
    DMM.LittleEndian -> LCLD.LittleEndian
    DMM.BigEndian -> LCLD.BigEndian

symbolicallyExecute
  :: CW.SymbolicExecutionContext
  -> IO CW.SymbolicExecutionResult
symbolicallyExecute (CW.BinarySymbolicExecutionContext sym symExConf binSymEx) = do
  let ?memOpts = LCLM.laxPointerMemOptions
  let ?recordLLVMAnnotation = \_ _ _ -> return ()

  let symArchFns = DMS.archFunctions archVals
  let crucRegsType = DMS.crucArchRegTypes symArchFns
  let regsRepr = LCT.StructRepr crucRegsType

  SomeOnlineSolver bak <- makeOnlineSolver sym (symExConf ^. CW.solver)

  -- TODO: Finish the memory setup

  -- We are using concrete mutable memory; this isn't generally safe, but is
  -- usually what users want and is a reasonable default. Users can add symbolic
  -- regions if they need to be more conservative.
  (memImpl, memPtrTbl) <-
    case CW.binaryDiscoveryInfo binSymEx of
      CW.DiscoveryInfoWith_ _ lb _ discState ->
        DMSM.newGlobalMemory (CW.binaryArchRepr binSymEx) bak (endianness discState) DMSM.ConcreteMutable (MBL.memoryImage lb)

  LCCC.SomeCFG cfg <-
    case CW.binaryDiscoveryFunInfo binSymEx of
      Some dfi -> toCrucibleCFG symArchFns dfi
  let arguments = LCS.RegMap (Ctx.singleton (binSymEx ^. CW.initialRegisters))
  let simAction = LCS.runOverrideSim regsRepr (LCS.regValue <$> LCS.callCFG cfg arguments)

  halloc <- CFH.newHandleAllocator
  memVar <- LCCC.freshGlobalVar halloc (DT.pack "crepitans-global-machine-code-memory") LCLM.memRepr
  let globals = LCSG.insertGlobal memVar memImpl LCS.emptyGlobals

  let globalMap = DMSM.mapRegionPointers memPtrTbl
  DMS.withArchEval archVals sym $ \archEvalFn -> do
    let extImpl = machineCodeExtension archEvalFn memVar globalMap

    -- We start with an empty set of bindings because we will be implementing
    -- incremental callee discovery and translation, so the call handler will
    -- populate this as-needed
    let fnBindings = LCS.FnBindings CFH.emptyHandleMap

    let ctx = LCS.initSimContext bak LCLI.llvmIntrinsicTypes halloc IO.stdout fnBindings extImpl DMS.MacawSimulatorState
    let s0 = LCS.InitialState ctx globals LCS.defaultAbortHandler regsRepr simAction
    let execFeatures = fmap LCS.genericToExecutionFeature (symExConf ^. CW.executionFeatures)
    res <- LCS.executeCrucible execFeatures s0
    let symRes = CW.SymbolicExecutionResult_ { CW.result = res
                                             , CW.resultRepr = regsRepr
                                             }
    return (CW.SymbolicExecutionResult bak symRes)
  where
    archVals = CW.binaryArchVals binSymEx

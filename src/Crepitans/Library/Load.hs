{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Crepitans.Library.Load (
    loadBinary
  , formatBinaryHeader
  , discoverFunctions
  , discoveredFunctions
  , functionAddress
  , functionName
  ) where

import           Control.Lens ( (&), (^.), (.~) )
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as DBU
import qualified Data.ElfEdit as DE
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Vector as DV
import qualified Lumberjack as LJ
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PRS

import qualified Data.Macaw.Architecture.Info as DMAI
import qualified Data.Macaw.ARM as DMA
import           Data.Macaw.ARM.ARMReg ()
import qualified Data.Macaw.BinaryLoader as DMB
import           Data.Macaw.BinaryLoader.PPC ()
import           Data.Macaw.BinaryLoader.AArch32 ()
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.CFG as DMC
import qualified Data.Macaw.Discovery as DMD
import qualified Data.Macaw.Memory as DMM
import qualified Data.Macaw.Memory.LoadCommon as DMML
import qualified Data.Macaw.PPC as DMP
import           Data.Macaw.PPC.PPCReg ()
import qualified Data.Macaw.X86 as DMX
import qualified Data.Macaw.Utils.IncComp as DMUI

import qualified Crepitans.Architecture as CArch
import qualified Crepitans.Exceptions as CE
import qualified Crepitans.Log as CL
import qualified Crepitans.WrapperTypes as CW

-- | Load a binary (of any supported format) from disk
loadBinary
  :: FilePath
  -- ^ The path of a binary to load
  -> IO CW.Binary
loadBinary p = do
  bytes <- BS.readFile p
  case DE.decodeElfHeaderInfo bytes of
    Left (offset, msg) -> CMC.throwM (CE.ELFHeaderDecodeError (fromIntegral offset) msg)
    Right (DE.SomeElf ehi) -> return (CW.ELFBinary ehi)

prettyELFHeader
  :: DE.ElfHeaderInfo w
  -> String
prettyELFHeader ehi = PRS.renderString (PP.layoutCompact doc)
  where
    ehdr = DE.header ehi
    doc = PP.vcat [ PP.pretty "ELF file"
                  , PP.pretty "Data format: " <> PP.viaShow (DE.headerData ehdr)
                  , PP.pretty "Header class: " <> PP.viaShow (DE.headerClass ehdr)
                  , PP.pretty "OS ABI: " <> PP.viaShow (DE.headerOSABI ehdr)
                  , PP.pretty "ABI Version: " <> PP.viaShow (DE.headerABIVersion ehdr)
                  , PP.pretty "Type: " <> PP.viaShow (DE.headerType ehdr)
                  , PP.pretty "Machine: " <> PP.viaShow (DE.headerMachine ehdr)
                  , case DE.headerClass ehdr of
                      DE.ELFCLASS32 -> PP.pretty "Entry address: " <> PP.viaShow (DE.headerEntry ehdr)
                      DE.ELFCLASS64 -> PP.pretty "Entry address: " <> PP.viaShow (DE.headerEntry ehdr)
                  , PP.pretty "Flags: " <> PP.viaShow (DE.headerFlags ehdr)
                  ]

-- | Return a string containing some basic header information from the binary header
--
-- Note that the format of this information is not guaranteed to remain stable.
formatBinaryHeader
  :: CW.Binary
  -> IO String
formatBinaryHeader b =
  case b of
    CW.ELFBinary ehi -> return (prettyELFHeader ehi)

logDiscoveryEvent
  :: (DMC.MemWidth (DMC.ArchAddrWidth arch))
  => LJ.LogAction IO CL.LogMessage
  -> CArch.ArchRepr arch
  -> DMD.DiscoveryEvent arch
  -> IO ()
logDiscoveryEvent logAction archRepr evt =
  LJ.writeLog logAction (CL.DiscoveryEvent archRepr evt)

resolveFunctions
  :: DMD.DiscoveryOptions
  -> DMD.DiscoveryState arch
  -> DMUI.IncCompM (DMD.DiscoveryEvent arch) r (DMD.DiscoveryState arch)
resolveFunctions disOpts ds0 = DMAI.withArchConstraints archInfo $ do
  case Map.minViewWithKey (ds0 ^. DMD.unexploredFunctions) of
    Nothing -> return ds0
    Just ((funcEntryAddr, reason), restUnexplored) -> do
      let ds1 = ds0 & DMD.unexploredFunctions .~ restUnexplored
      DMUI.incCompLog (DMD.ReportAnalyzeFunction funcEntryAddr)
      if Map.member funcEntryAddr (ds1 ^. DMD.funInfo)
        then resolveFunctions disOpts ds1
        else do
          (ds2, Some f) <- DMUI.liftIncComp id $ DMD.discoverFunction disOpts funcEntryAddr reason ds1 []
          DMUI.incCompLog (DMD.ReportAnalyzeFunctionDone f)
          resolveFunctions disOpts ds2
  where
    archInfo = DMD.archInfo ds0

incrementalDiscovery
  :: DMD.DiscoveryOptions
  -> DMD.DiscoveryState arch
  -> DMUI.IncCompM (DMD.DiscoveryEvent arch) r (DMD.DiscoveryState arch)
incrementalDiscovery disOpts ds0 = DMAI.withArchConstraints archInfo $ do
  let ds1 = ds0 & DMD.markAddrsAsFunction DMD.InitAddr (Map.keys (DMD.symbolNames ds0))
  resolveFunctions disOpts ds1
  where
    archInfo = DMD.archInfo ds0

loadedSymbols
  :: ( w ~ DMC.ArchAddrWidth arch
     , DMB.BinaryLoader arch binFmt
     )
  => DMB.LoadedBinary arch binFmt
  -> Map.Map (DMM.MemSegmentOff w) BS.ByteString
loadedSymbols lb = Map.fromList $
  [ (memSegOff, symName)
  | Just eps <- return (DMB.entryPoints lb)
  , memSegOff <- F.toList eps
  , Just symName <- return (DMB.symbolFor lb (DMM.segoffAddr memSegOff))
  ]

loadELFWith
  :: ( w ~ DMC.ArchAddrWidth arch
     , DMB.BinaryLoader arch (DE.ElfHeaderInfo w)
     )
  => LJ.LogAction IO CL.LogMessage
  -> DE.ElfHeaderInfo w
  -> CArch.ArchRepr arch
  -> DMAI.ArchitectureInfo arch
  -> IO CW.DiscoveryInfo
loadELFWith logAction ehi archRepr archInfo = do
  lb <- DMB.loadBinary DMML.defaultLoadOptions ehi

  DMAI.withArchConstraints archInfo $ do
    let addrSyms = loadedSymbols lb
    let s0 = DMD.emptyDiscoveryState (DMB.memoryImage lb) addrSyms archInfo
    DMUI.processIncCompLogs (logDiscoveryEvent logAction archRepr) $ DMUI.runIncCompM $ do
      s1 <- incrementalDiscovery DMD.defaultDiscoveryOptions s0
      let binfo = CW.DiscoveryInfoWith_ (CW.ELFBinary ehi) lb archRepr s1
      return (CW.DiscoveryInfoWith binfo)

discoverFunctions
  :: LJ.LogAction IO CL.LogMessage
  -> CW.Binary
  -> IO CW.DiscoveryInfo
discoverFunctions logAction bin =
  case bin of
    CW.ELFBinary ehi ->
      let hdr = DE.header ehi
      in case (DE.headerClass hdr, DE.headerMachine hdr) of
        (DE.ELFCLASS64, DE.EM_X86_64) -> loadELFWith logAction ehi CArch.X86_64 DMX.x86_64_linux_info
        (DE.ELFCLASS64, DE.EM_PPC64) -> do
          lb <- DMB.loadBinary DMML.defaultLoadOptions ehi
          loadELFWith logAction ehi CArch.PPC64 (DMP.ppc64_linux_info lb)
        (DE.ELFCLASS32, DE.EM_PPC) -> loadELFWith logAction ehi CArch.PPC32 DMP.ppc32_linux_info
        (DE.ELFCLASS32, DE.EM_ARM) -> loadELFWith logAction ehi CArch.AArch32 DMA.arm_linux_info
        (klass, mach) -> CMC.throwM (CE.UnsupportedELFArchitecture klass mach)

discoveredFunctions
  :: CW.DiscoveryInfo
  -> IO (DV.Vector CW.Function)
discoveredFunctions (CW.DiscoveryInfoWith di@(CW.DiscoveryInfoWith_ _bin _lb _archRepr ds)) =
  return (DV.fromList [ CW.BinaryFunctionWith di dfi
                      | Some dfi <- Map.elems (ds ^. DMD.funInfo)
                      ])

functionAddress
  :: CW.Function
  -> IO CW.Address
functionAddress (CW.BinaryFunctionWith (CW.DiscoveryInfoWith_ _bin _lb archRepr _ds) dfi) =
  return (CW.SegmentOffset archRepr (DMD.discoveredFunAddr dfi))

-- | Extract the name of a function
--
-- Note that function names are both optional and bytestrings. This function
-- with synthesize a name based on the address if there is no natural name. If
-- the name cannot be decoded as utf8, invalid bytes will be replaced with a
-- default.
functionName
  :: CW.Function
  -> IO String
functionName (CW.BinaryFunctionWith (CW.DiscoveryInfoWith_ {}) dfi) =
  return $ maybe def DBU.toString (DMD.discoveredFunSymbol dfi)
  where
    def = "func" ++ show (DMD.discoveredFunAddr dfi)

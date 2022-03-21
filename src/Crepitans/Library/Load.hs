{-# LANGUAGE GADTs #-}
module Crepitans.Library.Load (
  loadBinary
  , formatBinaryHeader
  ) where

import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as DE
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PRS

import qualified Data.Macaw.BinaryLoader as DMB

import qualified Crepitans.ArgumentMapping as CA
import qualified Crepitans.Exceptions as CE

-- | Load a binary (of any supported format) from disk
loadBinary
  :: FilePath
  -- ^ The path of a binary to load
  -> IO CA.Binary
loadBinary p = do
  bytes <- BS.readFile p
  case DE.decodeElfHeaderInfo bytes of
    Left (offset, msg) -> CMC.throwM (CE.ELFHeaderDecodeError (fromIntegral offset) msg)
    Right (DE.SomeElf ehi) -> return (CA.ELFBinary ehi)

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
  :: CA.Binary
  -> IO String
formatBinaryHeader b =
  case b of
    CA.ELFBinary ehi -> return (prettyELFHeader ehi)

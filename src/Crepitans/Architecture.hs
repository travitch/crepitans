{-# LANGUAGE GADTs #-}
module Crepitans.Architecture (
  ArchRepr(..)
  ) where

import qualified Data.Macaw.ARM as DMA
import qualified Data.Macaw.PPC as DMP
import qualified Data.Macaw.X86 as DMX

data ArchRepr arch where
  PPC32 :: ArchRepr DMP.PPC32
  PPC64 :: ArchRepr DMP.PPC64
  AArch32 :: ArchRepr DMA.ARM
  X86_64 :: ArchRepr DMX.X86_64

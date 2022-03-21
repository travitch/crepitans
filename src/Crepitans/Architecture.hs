{-# LANGUAGE GADTs #-}
module Crepitans.Architecture (
  ArchRepr(..)
  ) where

import qualified Data.Macaw.ARM as DMA
import qualified Data.Macaw.PPC as DMP
import qualified Data.Macaw.X86 as DMX

-- | A run-time representative that enables recovering the architecture of a binary
--
-- Note that unlike many of our uses of GADTs, this one doesn't use a closed
-- data kind for its type parameter. That is because the types of each
-- architecture are not related in any way (except for PowerPC), so a closed
-- universe of types would not work.
data ArchRepr arch where
  PPC32 :: ArchRepr DMP.PPC32
  PPC64 :: ArchRepr DMP.PPC64
  AArch32 :: ArchRepr DMA.ARM
  X86_64 :: ArchRepr DMX.X86_64

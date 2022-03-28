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
    Argument(..)
  , ArgumentRepr(..)
  , ArgumentMapping(..)
  , ArgumentTypeError(..)
  , mapToHaskell
  ) where

import qualified Control.Monad.Except as CME
import           Control.Monad.Trans ( lift )
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Vector as DV
import qualified Prettyprinter as PP

import qualified Crepitans.Panic as CP
import           Crepitans.WrapperTypes

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
  SymbolicExecutionContext :: SymbolicExecutionContext -> Argument SymbolicExecutionContextK
  SymbolicExecutionResult :: SymbolicExecutionResult -> Argument SymbolicExecutionResultK
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

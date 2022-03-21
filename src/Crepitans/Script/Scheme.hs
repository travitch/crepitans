{-# LANGUAGE GADTs #-}
module Crepitans.Script.Scheme (
  run
  ) where

import qualified Control.Monad.Catch as CMC
import qualified Control.Monad.Except as CME
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans ( lift )
import qualified Data.Dynamic as DD
import qualified Data.Foldable as F
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Parser as LSP
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV
import qualified Lumberjack as LJ

import qualified Crepitans.ArgumentMapping as CA
import qualified Crepitans.Exceptions as CE
import qualified Crepitans.Library.Scripting as CLS
import qualified Crepitans.Log as CL

lispToHaskell :: LST.LispVal -> CA.ArgumentRepr tp -> IO (Maybe (CA.Argument tp))
lispToHaskell lv rep =
  case (rep, lv) of
    (CA.PathRepr, LST.String fp) -> return (Just (CA.Path fp))
    (CA.BinaryRepr, LST.Opaque dyn)
      | Just bin <- DD.fromDynamic dyn -> return (Just (CA.Binary bin))
    _ -> return Nothing

haskellToLisp :: CA.Argument tp -> LST.LispVal
haskellToLisp a =
  case a of
    CA.Binary bin -> LST.Opaque (DD.toDyn bin)
    CA.String_ s -> LST.String s
    CA.Path s -> LST.String s

lispValueName :: LST.LispVal -> String
lispValueName lv =
  case lv of
    LST.Atom {} -> "atom"
    LST.List {} -> "list"
    LST.DottedList {} -> "list(.)"
    LST.Vector {} -> "vector"
    LST.ByteVector {} -> "bytevector"
    LST.HashTable {} -> "hashtable"
    LST.Number {} -> "number"
    LST.Float {} -> "float"
    LST.Complex {} -> "complex"
    LST.Rational {} -> "rational"
    LST.String {} -> "string"
    LST.Char {} -> "char"
    LST.Bool {} -> "bool"
    LST.PrimitiveFunc {} -> "primitive-func"
    LST.Func {} -> "func"
    LST.HFunc {} -> "hfunc"
    LST.Nil {} -> "nil"
    LST.EOF -> "eof"
    LST.LispEnv {} -> "env"
    LST.SyntaxExplicitRenaming {} -> "syntax-explicit-renaming"
    LST.Syntax {} -> "syntax"
    LST.Continuation {} -> "continuation"
    LST.Port {} -> "port"
    LST.Opaque {} -> "opaque"
    LST.Pointer {} -> "pointer"
    LST.CustFunc {} -> "custom-func"
    LST.EvalFunc {} -> "eval-func"
    LST.IOFunc {} -> "io-func"

schemeMapping :: CA.ArgumentMapping IO LST.LispVal
schemeMapping =
  CA.ArgumentMapping { CA.toHaskell = lispToHaskell
                     , CA.fromHaskell = haskellToLisp
                     , CA.valueTypeName = lispValueName
                     }

newtype Program = Program { asLispVals :: [LST.LispVal] }
  deriving (Show)

parseScript :: (CMC.MonadThrow m) => DT.Text -> m Program
parseScript scriptContents =
  case LSP.readExprList (DT.unpack scriptContents) of
    Left lispError -> CMC.throwM (CE.LispParseException lispError)
    Right vals -> return (Program vals)

data FunctionDefinition where
  F :: String -> Ctx.Assignment CA.ArgumentRepr tps -> (Ctx.Assignment CA.Argument tps -> IO (CA.Argument r)) -> FunctionDefinition

functionDefinitionName :: FunctionDefinition -> String
functionDefinitionName (F name _ _) = name

toFunc
  :: CA.ArgumentMapping IO LST.LispVal
  -> FunctionDefinition
  -> LST.LispVal
toFunc mapping (F name argumentReprs f) = LST.CustFunc $ \actuals -> do
  eres <- lift $ CA.mapToHaskell mapping name (DV.fromList actuals) argumentReprs
  case eres of
    Right wrappedActuals -> lift (CA.fromHaskell mapping <$> f wrappedActuals)
    Left err -> CMC.throwM (CE.ArgumentMappingError err)

libraryFunctions :: [FunctionDefinition]
libraryFunctions =
  [ F "load-binary" (Ctx.Empty Ctx.:> CA.PathRepr) CLS.loadBinary
  , F "format-binary-header" (Ctx.Empty Ctx.:> CA.BinaryRepr) CLS.formatBinaryHeader
  ]

-- | Introduce all of the value and function bindings into the Scheme
-- environment to support evaluation
initializeEnvironment
  :: (MonadIO m, CMC.MonadThrow m)
  => LST.Env
  -> m ()
initializeEnvironment initialEnv = do
  eres <- liftIO $ CME.runExceptT $ do
    F.forM_ libraryFunctions $ \lf -> do
      _ <- LSV.defineVar initialEnv (functionDefinitionName lf) (toFunc schemeMapping lf)
      return ()
  case eres of
    Left err -> CMC.throwM (CE.LispEvaluationError err)
    Right _ -> return ()


-- | Run parse the script as a Scheme script and run it
--
-- The evaluator will stream output via the 'LJ.LogAction'. Note that any fatal
-- errors will be raised as an exception, which is intended to be caught by the
-- caller.
run
  :: (MonadIO m, CMC.MonadThrow m)
  => LJ.LogAction IO CL.LogMessage
  -- ^ The logger to use; note that essentially all program output is emitted through this structured logging interface
  -> FilePath
  -- ^ The name of the input file for diagnostic purposes
  -> DT.Text
  -- ^ The contents of the script to execute
  -> m ()
run logAction scriptPath scriptContents = do
  prog <- parseScript scriptContents
  env <- liftIO LSC.r7rsEnv'
  initializeEnvironment env
  -- Note that this evaluator throws away all of the values returned by each
  -- top-level value (e.g., function application).  This is the intended
  -- semantics; if users want to save those values they need to bind them to
  -- global variables.
  eres <- liftIO $ CME.runExceptT $ mapM_ (LSC.evalLisp env) (asLispVals prog)
  case eres of
    Right _ -> return ()
    Left lispErr -> CMC.throwM (CE.LispEvaluationError lispErr)

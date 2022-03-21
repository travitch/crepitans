{-# LANGUAGE GADTs #-}
-- | Tools for fingerprinting scripts by their programming language, meant to be
-- used to help decide which interpreter to use
module Crepitans.Script.Fingerprint (
    ScriptLanguage(..)
  , fingerprint
  ) where

import qualified Data.Text as DT
import qualified System.FilePath as SF

-- | Possible supported scripting languages
data ScriptLanguage where
  Scheme :: ScriptLanguage
  Python :: ScriptLanguage
  CodeQL :: ScriptLanguage

byExtension :: FilePath -> Maybe ScriptLanguage
byExtension fp =
  case SF.takeExtension fp of
    ".py" -> Just Python
    ".ss" -> Just Scheme
    ".scm" -> Just Scheme
    _ -> Nothing

-- | Attempt to fingerprint a file based on its scripting language
--
-- Start by examining the filename.
--
-- TODO: Examine the file contents if needed
fingerprint
  :: FilePath
  -- ^ Script filename
  -> DT.Text
  -- ^ Script contents
  -> Maybe ScriptLanguage
fingerprint fp _contents =
  byExtension fp

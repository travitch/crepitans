module Main ( main ) where

import qualified Data.Text.IO as DTI
import qualified Options.Applicative as OA

import qualified Logger as L
import qualified Options as O

import qualified Crepitans as C


mainWithOptions :: O.Options -> IO ()
mainWithOptions opts = do
  logger <- L.makeLogger opts
  let inputFile = O.scriptFile opts
  t <- DTI.readFile inputFile
  C.runAnalysis (L.logger logger) inputFile t
  L.finishLogger logger

main :: IO ()
main = OA.execParser p >>= mainWithOptions
  where
    p = OA.info (OA.helper <*> O.parser)
     (  OA.fullDesc
     <> OA.progDesc "Explore binary programs through a scripting interface"
     )

module Options (
    Options(..)
  , parser
  ) where

import qualified Options.Applicative as OA

data Options =
  Options { scriptFile :: FilePath
          }

parser :: OA.Parser Options
parser = Options <$> OA.strArgument ( OA.metavar "FILE"
                                   <> OA.help "The script file to load and execute"
                                    )

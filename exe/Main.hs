module Main (main) where

import Control.Concurrent.MC
import Control.Concurrent.STM
import Data.Version
import Options.Applicative
import Paths_mc
import System.IO

main :: IO ()
main = do
  CLI qks <- customExecParser prefs' $ pinfo $ showVersion version
  qkVar <- case qks of
    [qk] -> newTVarIO qk
    _    -> fail "invalid quit key"
  hSetBuffering stdin NoBuffering
  mc qkVar =<< newTVarIO []

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "master of ceremonies"

data CLI = CLI String

parser :: Parser CLI
parser = CLI
  <$> option auto (short 'q' <> long "quit" <> value "q" <> showDefault <> help "quit key")

module Main (main) where

import Control.Concurrent.MC
import Control.Concurrent.STM
import Control.Monad
import Data.Version
import Options.Applicative
import Paths_mc
import System.IO
import System.Process

main :: IO ()
main = do
  CLI cnt qks kmp <- customExecParser prefs' $ pinfo $ showVersion version
  qkVar <- case qks of
    [qk] -> newTVarIO qk
    _    -> fail "invalid quit key"
  kmIO <- forM kmp $ \kv -> case kv of
    ([k], cmd) -> return (k, callCommand cmd)
    _ -> fail "invalid key map"
  hSetBuffering stdin NoBuffering
  mc cnt qkVar =<< newTVarIO kmIO

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "master of ceremonies"

data CLI = CLI Int String [(String, String)]

parser :: Parser CLI
parser = CLI
  <$> option auto (short 'c' <> long "conc" <> metavar "INT" <> value 64
        <> showDefault <> help "concurrency factor")
  <*> strOption (short 'q' <> long "quit" <> metavar "CHR" <> value "q"
        <> showDefault <> help "quit key")
  <*> many ((,)
        <$> strOption (short 'k' <> metavar "CHR" <> help "key")
        <*> strOption (short 'v' <> metavar "CMD" <> help "cmd"))

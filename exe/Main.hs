module Main (main) where

import Control.Concurrent.MC
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Version
import Options.Applicative
import Paths_mc
import System.IO
import System.Process

main :: IO ()
main = do
  CLI c qs kvs <- customExecParser prefs' $ pinfo $ showVersion version
  q <- checkChar qs
  km <- forM kvs $ \(ks, cmd) -> checkChar ks >>= \k ->
    return (k, putStrLn (k:"!") >> callCommand cmd)
  kv <- newTVarIO km
  hSetBuffering stdin NoBuffering
  bracket (hGetEcho stdin <* hSetEcho stdin False) (hSetEcho stdin) $ const $
    mc c (Just q) (\k -> putStrLn (k:"?")) kv getChar

checkChar :: String -> IO Char
checkChar [c] = return c
checkChar cs  = fail $ "invalid char: " ++ cs

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

module Main where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import SimpleFormulaChecker (compareSpec, Mode (..))

import Server (runApi)

-- | Command-line options.
data Options = Options
  { srcA      :: String
  , srcB      :: String
  , method1   :: String
  , method2   :: String
  , runServer :: Bool
  , port      :: Int
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (  long "srcA"
      <> showDefault
      <> value "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"
      <> metavar "STRING"
      <> help "Java source file for A"
      )
  <*> strOption
      (  long "srcB"
      <> showDefault
      <> value "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"
      <> metavar "STRING"
      <> help "Java source file for B"
      )
  <*> strOption
      (  short 'a'
      <> value "_default_"
      <> metavar "STRING"
      <> help "First method"
      )
  <*> strOption
      (  short 'b'
      <> value "_default_"
      <> metavar "STRING"
      <> help "Second method"
      )
  <*> switch
      (  short 'w'
      <> long "runServer"
      <> help "Run server"
      )
  <*> option auto
      (  short 'p'
      <> long "port"
      <> metavar "INT"
      <> showDefault
      <> value 8888
      <> help "Listening port"
      )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Main.
main :: IO ()
main = runMain =<< execParser (parseOptions `withInfo` "Java WLP")

-- | Run.
runMain :: Options -> IO ()
runMain (Options srcA srcB methodA methodB runServer port) =
  if runServer then
    runApi
  else do
    when (methodA == "_default_") $ fail "No files given."
    response <- compareSpec Release (srcA, methodA) (srcB, methodB)
    print response

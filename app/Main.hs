module Main where

import SimpleFormulaChecker (compareSpec)
import Data.Semigroup ((<>))
import Options.Applicative
import Control.Monad

-- | Command-line options.
data Options = Options
  { src :: String
  , method1 :: String
  , method2 :: String
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> option auto
      (  long "src"
      <> showDefault
      <> value "examples/javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"
      <> metavar "STRING"
      <> help "Java source file"
      )
  <*> strOption
      (  short 'a'
      <> metavar "STRING"
      <> help "First method"
      )
  <*> strOption
      (  short 'b'
      <> metavar "STRING"
      <> help "Second method"
      )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Java WLP")

-- | Run.

run :: Options -> IO ()
run (Options src method1 method2) = void $
  compareSpec (src, method1) (src, method2)

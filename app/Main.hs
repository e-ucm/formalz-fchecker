module Main where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative

import API (Options (..), Mode (..), ParseMode (..), compareSpec)
import Server (runApi)

-- | Command-line options.
data MainOptions = MainOptions
  { sourceA   :: String
  , sourceB   :: String
  , methodA   :: String
  , methodB   :: String
  , runServer :: Bool
  , port      :: Int
  , mode      :: Mode
  }

-- | Parsing of command-line options.
parseOptions :: Parser MainOptions
parseOptions = MainOptions
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
  <*> (toMode <$> switch
      (  short 'd'
      <> long  "debugMode"
      <> help "Run in debug mode (instead of release)"
      ))

toMode :: Bool -> Mode
toMode False = Release
toMode True  = Debug

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Main.
main :: IO ()
main = runMain =<< execParser (parseOptions `withInfo` "Java WLP")

-- | Run.
runMain :: MainOptions -> IO ()
runMain (MainOptions srcA srcB methA methB serverFlag portNo runMode) =
  if serverFlag then
    runApi portNo
  else do
    when (methA == "_default_") $ fail "No files given."
    response <- compareSpec (Options runMode File True True) (srcA, methA) (srcB, methB)
    print response

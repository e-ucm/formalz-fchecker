import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TExamples
import TIRParser
import TEquivalenceClasses
import TModelParser
import TFeedback
import TNormalizer
import TServer

main :: IO ()
main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <-
      [ ("LIR_PARSER", parserTests)
      , ("MODEL_PARSER", modelParserTests)
      , ("EXAMPLES", examples)
      , ("EQUIV_REAL", genEquivTests "test/test_files/Reals.java")
      , ("EQUIV_ARRAY", genEquivTests "test/test_files/Arrays.java")
      , ("EQUIV_ARRAY2D", genEquivTests "test/test_files/Arrays2d.java")
      , ("FEEDBACK", feedbackTests)
      , ("NORMALIZER", normTests)
      , ("SERVER", serverTests)
      , ("DESUGAR", desugarTests)
      ]
  ]
  where
    constructTestSuite s suite
      = testGroup s [ testCase (s ++ "_" ++ show i) t
                    | (i :: Int, t) <- zip [1..] suite
                    ]

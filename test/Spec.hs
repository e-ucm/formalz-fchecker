import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TExamples
import TIRParser
import TEquivalenceClasses
import TModelParser
import TFeedback

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [
        ("LIR_PARSER", parserTests)
      , ("MODEL_PARSER", modelParserTests)
      , ("EXAMPLES", examples)
      , ("EQUIV_REAL", genEquivTests "examples/test_equiv/Reals.java")
      , ("EQUIV_ARRAY", genEquivTests "examples/test_equiv/Arrays.java")
      , ("FEEDBACK", feedbackTests)
      ]
  ]
  where
    constructTestSuite s suite =
      testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

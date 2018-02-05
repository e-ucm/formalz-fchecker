import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TExamples
import TIRParser
import TIRTest
import TEquivalenceClasses
import TZ3Model

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [
        ("LIR_PARSER", parserTests)
      , ("Z3_MODEL", modelTests)
      , ("EXAMPLES", examples)
      , ("AUTOMATED_TESTING_CHECK", testingModuleTests)
      , ("EQUIV_REAL", genEquivTests "examples/test_equiv/Reals.java")
      , ("EQUIV_ARRAY", genEquivTests "examples/test_equiv/Arrays.java")
      ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

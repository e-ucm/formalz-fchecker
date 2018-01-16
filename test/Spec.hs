import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TExamples
import TIRParser
import TIRTest

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("EXAMPLES", equivalenceTests)
                             , ("LIR_PARSER", parserTests)
                             , ("AUTOMATED_TESTING_CHECK", testingModuleTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

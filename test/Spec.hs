import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TExamples
import TIRParser
import TIRQuickCheck

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("EXAMPLES", equivalenceTests)
                             , ("LIR_PARSER", parserTests)
                             , ("QUICK_CHECK", quickCheckTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]

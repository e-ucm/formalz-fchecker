module TEquivalenceClasses where

import Data.List (elemIndex)
import Data.List.Split (splitOn)
import System.IO.Silently (silence)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Test.HUnit

import SimpleFormulaChecker (compareSpec, parseMethodIds)

testEquiv :: Bool -> String -> String -> String -> Assertion
testEquiv b src s s' =
  unsafePerformIO (id $ compareSpec (src, s) (src, s')) @?= b
(.==) = testEquiv True
(.!=) = testEquiv False

genEquivTests edslSrc =
  let methodIds = unsafePerformIO (silence $ parseMethodIds edslSrc)
      getClass = last . splitOn "_"
      tailFrom :: Eq a => [a] -> a -> [a]
      tailFrom xs x = case elemIndex x xs of Just i  -> snd $ splitAt i xs
                                             Nothing -> []
  in [ a `op` b | a <- methodIds
                , b <- methodIds `tailFrom` a
                , a /= b
                , let op = unsafePerformIO $ do
                        let eq = getClass a == getClass b
                        putStrLn $ foldl1 (++)
                          ["  (", a, if eq then " == " else " != ", b, ")"]
                        return $ (if eq then (.==) else (.!=)) edslSrc
                ]

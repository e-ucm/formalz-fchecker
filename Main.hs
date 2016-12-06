module Main where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Data.List
import System.FilePath(joinPath)


import WLP
import Verifier
import HelperFunctions
import Settings


main :: IO ()
main = do
    source <- readFile (joinPath ["Tests", testFile ++ ".java"])
    
    let result = parser compilationUnit source
    
    case result of
        Left error -> print error
        Right compUnit -> do
                            putStrLn "\r\n-----Code-----"
                            print compUnit
                            putStrLn "\r\n-----WLP-----"
                            let pred = wlpWithEnv (getDecls compUnit) (getStaticVars compUnit) (getMainMethod (getDecls compUnit)) postCond'
                            putStrLn . prettyPrint $ pred
                            putStrLn "\r\n-----Correctness-----"
                            if unsafeIsTrue pred then putStrLn "WLP evaluates to true" else (if unsafeIsFalse pred then putStrLn "WLP evaluates to false" else putStrLn "undecidable")
                            
                            


-- The post-condition (for testing)
postCond' :: Exp
postCond' = case parser Language.Java.Parser.exp postCond of
            Right e -> e
            _       -> error "syntax error in post-condition"
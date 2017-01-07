module Main where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Data.List
import Control.Monad
import System.FilePath(joinPath)


import WLP
import Types
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
                            let decls = getDecls compUnit
                            let methods = getMethodIds decls
                            let env = getTypeEnv compUnit decls methods
                         --   putStrLn "\r\n-----Code-----"
                         --   print compUnit
                            mapM_ (testMethod compUnit env decls) methods
                            
-- | Calculates and prints the wlp for a given method
testMethod :: CompilationUnit -> TypeEnv -> [TypeDecl] -> Ident -> IO ()
testMethod compUnit env decls id = do
    putStrLn "--------------"
    putStrLn (prettyPrint id ++ "\r\n")
    let pred = wlpWithEnv decls env (fromJust' "testMethod" $ getMethod decls id) postCond'
    putStrLn . prettyPrint $ pred
    putStrLn ""
    if unsafeIsTrue env pred then putStrLn "WLP evaluates to true" else (if unsafeIsFalse env pred then putStrLn "WLP evaluates to false" else putStrLn "undecidable")
    putStrLn "--------------"

-- The post-condition (for testing)
postCond' :: Exp
postCond' = case parser Language.Java.Parser.exp postCond of
            Right e -> e
            _       -> error "syntax error in post-condition"
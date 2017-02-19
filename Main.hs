{-# LANGUAGE TupleSections #-}

module Main where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Data.List
import Control.Monad
import System.FilePath(joinPath)
import System.Directory

import WLP
import Types
import Verifier
import HelperFunctions
import Settings

sourcePath, mutantsDir, resultsPath :: FilePath
sourcePath = joinPath ["Tests", testFile ++ ".java"]
mutantsDir = joinPath ["..", testFile ++ " mutants"]
resultsPath = joinPath ["Results", testFile ++ "_" ++ postCondVoid ++ "_" ++ postCondRefType ++ "_" ++ postCondPrimType ++ "_" ++ show ignoreLibMethods ++ "_" ++ show ignoreMainMethod ++ "_" ++ show nrOfUnroll]

-- The main functions performs the mutation test on the test file, given that the mutations of the source are already created in the right location
main :: IO ()
main = do
    -- Create the file for the results
    initResultsFile
    
    -- Parse the original sourceCode
    (env, decls, methods) <- parseFile sourcePath
    
    -- Get the wlp per method of the original source code
    wlpOriginal <- wlpMethods env decls methods
    
    -- Get the names of the folders containing the mutants (MAJOR creates a folder for every mutant)
    mutationFolders <- listDirectory mutantsDir
    
    -- Map each folder name to the path containing the mutant (mutant name is the same as the original file name)
    let mutationPaths = map (\n -> joinPath [mutantsDir, n, classPath testFile, testFile ++ ".java"]) mutationFolders
    
    -- Calculate the wlp per method of all the mutants
    wlpMutants <- mapM (\path -> parseFile path >>= (\(env', decls', methods') -> wlpMethods env' decls' methods') >>= return . (getMutantNumber path, )) mutationPaths
    
    -- A list containing a 1 or 0 per mutant, indicating the number of errors found
    errorsFound <- mapM (compareWlps resultsPath env decls wlpOriginal) wlpMutants
    
    printAndAppend resultsPath ("Total number of mutants: " ++ show (length errorsFound))
    printAndAppend resultsPath ("Number of mutants in which we found an error: " ++ show (sum errorsFound))
    
-- Creates and initializes a file for the results
initResultsFile :: IO ()
initResultsFile = do
    writeFile resultsPath ("testFile: " ++ testFile)
    printAndAppend resultsPath ("postCondVoid: " ++ postCondVoid)
    printAndAppend resultsPath ("postCondRefType: " ++ postCondRefType)
    printAndAppend resultsPath ("postCondPrimType: " ++ postCondPrimType)
    printAndAppend resultsPath ("ignoreLibMethods: " ++ show ignoreLibMethods)
    printAndAppend resultsPath ("ignoreMainMethod: " ++ show ignoreMainMethod)
    printAndAppend resultsPath ("nrOfUnroll: " ++ show nrOfUnroll)
    printAndAppend resultsPath ("erronous mutations detected:")
    
-- Prints a string and writes it to the results file
printAndAppend :: FilePath -> String -> IO ()
printAndAppend results s = do
    appendFile results ("\n" ++ s)
    putStrLn s

-- Parses a files and extracts the necessary information from the compilation unit
parseFile :: FilePath -> IO (TypeEnv, [TypeDecl], [Ident])
parseFile s = do
    -- Get the source code
    source <- readFile s
    
    -- Parse the source code
    case parser compilationUnit source of
        Left parseError -> error (show parseError)
        Right compUnit -> do
                            let decls = getDecls compUnit
                            let methods = getMethodIds decls
                            let env = getTypeEnv compUnit decls methods
                            
                            return (env, decls, methods)
    
-- | Calculates the wlp for every method in the list seperately
wlpMethods :: TypeEnv -> [TypeDecl] -> [Ident] -> IO [(Ident, Exp)]
wlpMethods env decls methods = mapM (\ident -> wlpMethod env decls ident >>= return . (ident, )) methods' where
    methods' = if ignoreMainMethod then filter (/= Ident "main") methods else methods
                            
-- | Calculates the wlp for a given method
wlpMethod :: TypeEnv -> [TypeDecl] -> Ident -> IO Exp
wlpMethod env decls ident = do
    -- Find the return type of the method
    let returnType = getMethodType decls ident
    
    -- Add returnValue to the type environment with the correct type
    let env' = extendEnv env decls ident
                    
    -- Calculate the wlp. Note that the post condition might depend on the type of the function
    let pred = wlpWithEnv decls env' (fromJust' "wlpMethod" $ getMethod decls ident) (getPostCond returnType)
    return pred
    
-- | Compares the wlp per method of a program S and mutation S' by verifying that (wlp (S, m) q => wlp (S', m) q) holds for every method m. Returns 0 if it holds and 1 if it doesn't hold
compareWlps :: FilePath -> TypeEnv -> [TypeDecl] -> [(Ident, Exp)] -> (String, [(Ident, Exp)]) -> IO Int
compareWlps results env decls s (path, s') = do
    -- Get a list with for every method the number of errors found (1 or 0)
    errorsFound <- mapM compareMethod s
    -- Return 1 if we found at least 1 error
    return (if sum errorsFound > 0 then 1 else 0)
        where 
        compareMethod (ident, e) = case lookup ident s' of
                                    Nothing -> putStrLn ("The method \'" ++ prettyPrint ident ++ "\' is missing in mutation " ++ path) >> return 0 -- print a warning and return 0 errors found
                                    Just e' -> if unsafeIsTrue (extendEnv env decls ident) decls (e `imp` e') then return 0 else printAndAppend results (path ++ " " ++ prettyPrint ident) >> return 1 -- print a message and return 1 error found

-- Gets the right post-condition given the type of a method
getPostCond :: Maybe Type -> Exp
getPostCond t = case parser Language.Java.Parser.exp postCond' of
                    Right e -> e
                    _       -> error "syntax error in post-condition"
    where postCond' = case t of
                        Nothing             -> postCondVoid
                        Just (RefType _)    -> postCondRefType
                        Just (PrimType _)   -> postCondPrimType
                    
-- Gets the mutant number (as a string) of a generated mutation
getMutantNumber :: FilePath -> String
getMutantNumber path = takeWhile (/= '\\') (path \\ (mutantsDir ++ "\\"))




----------------------------
--  Other test functions  --
----------------------------

-- Performs the false-positives-test on the set of test programs with equivalent mutations
testFalsePositives :: IO ()
testFalsePositives = do
    let results = joinPath ["Results", "False Positives", postCondVoid ++ "_" ++ postCondRefType ++ "_" ++ postCondPrimType ++ "_" ++ show ignoreLibMethods ++ "_" ++ show ignoreMainMethod ++ "_" ++ show nrOfUnroll]
    
    -- Create the file for the results
    writeFile results ("False positives test")
    printAndAppend results ("postCondVoid: " ++ postCondVoid)
    printAndAppend results ("postCondRefType: " ++ postCondRefType)
    printAndAppend results ("postCondPrimType: " ++ postCondPrimType)
    printAndAppend results ("ignoreLibMethods: " ++ show ignoreLibMethods)
    printAndAppend results ("ignoreMainMethod: " ++ show ignoreMainMethod)
    printAndAppend results ("nrOfUnroll: " ++ show nrOfUnroll)
    printAndAppend results ("False positives found in:")
    
    n1 <- testFalsePositives' results "BST.java" ["BST_no_parent.java"]
    n2 <- testFalsePositives' results "Fibonacci.java" ["Fibonacci_no_extra_prints.java"]
    n3 <- testFalsePositives' results "Stack.java" ["Stack_bool_is_result.java", "Stack_constructor_duplication.java", "Stack_useless_property.java"]
    n4 <- testFalsePositives' results "2D_to_1D.java" ["2D_to_1D_no_counter.java", "2D_to_1D_no_1D.java"]
    n5 <- testFalsePositives' results "Vectors01Generator.java" ["Vectors01Generator_no_for.java", "Vectors01Generator_print_string.java"]
    n6 <- testFalsePositives' results "MinsMaxs.java" ["MinsMaxs_R1.java", "MinsMaxs_R2.java", "MinsMaxs_R3.java"]
    n7 <- testFalsePositives' results "Normalizer.java" ["Normalizer_R1.java", "Normalizer_R2.java", "Normalizer_R3.java", "Normalizer_R4.java"]
    n8 <- testFalsePositives' results "Vector.java" ["Vector_R1.java", "Vector_R2.java", "Vector_R3.java"]
    
    printAndAppend results ("Total number of false positives: " ++ show (n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8))
    where
    testFalsePositives' :: FilePath -> FilePath -> [FilePath] -> IO Int
    testFalsePositives' results source mutations = testFalsePositives'' results (joinPath ["Equivalent mutations", source]) (map (\mutant -> joinPath ["Equivalent mutations", "Mutants", mutant]) mutations)
    
    -- Tests a specific source file against all its equivalent mutations, updates the test result file and returns the number of false positives
    testFalsePositives'' :: FilePath -> FilePath -> [FilePath] -> IO Int
    testFalsePositives'' results source mutations = do
        -- Parse the original sourceCode
        (env, decls, methods) <- parseFile source
        
        -- Get the wlp per method of the original source code
        wlpOriginal <- wlpMethods env decls methods
        
        -- Calculate the wlp per method of all the mutants
        wlpMutants <- mapM (\path -> parseFile path >>= (\(env', decls', methods') -> wlpMethods env' decls' methods') >>= return . (path, )) mutations
        
        -- A list containing a 1 or 0 per mutant, indicating the number of errors found
        -- CompareWlp also writes detailed information to the results file
        errorsFound <- mapM (compareWlps results env decls wlpOriginal) wlpMutants
        
        return (sum errorsFound)

-- Calculate the wlp of the test file (for testing purposes)
calcWlp :: IO ()
calcWlp = do
    source <- readFile sourcePath
    
    let result = parser compilationUnit source
    
    case result of
        Left error -> print error
        Right compUnit -> do
                            let decls = getDecls compUnit
                            let methods = if ignoreMainMethod then filter (/= Ident "main") (getMethodIds decls) else getMethodIds decls
                            let env = getTypeEnv compUnit decls methods
                            let printMethodWlp = (\ident e -> putStrLn (prettyPrint ident ++ ":\r\n" ++ prettyPrint e ++ "\r\n" ++ (if unsafeIsTrue (extendEnv env decls ident) decls e then "WLP evaluates to true" else (if unsafeIsFalse (extendEnv env decls ident) decls e then "WLP evaluates to false" else "undecidable")) ++ "\r\n"))
                            mapM_ (\ident -> wlpMethod env decls ident >>= printMethodWlp ident) methods
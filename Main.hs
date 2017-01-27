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

sourcePath, mutantsDir :: FilePath
sourcePath = joinPath ["Tests", testFile ++ ".java"]
mutantsDir = joinPath ["..", testFile ++ " mutants"]

main :: IO ()
main = do
    -- Parse the original sourceCode
    (env, decls, methods) <- parseFile sourcePath
    
    -- Get the wlp per method of the original source code
    wlpOriginal <- wlpMethods env decls methods
    
    -- Get the names of the folders containing the mutants (MAJOR creates a folder for every mutant)
    mutationFolders <- listDirectory mutantsDir
    
    -- Map each folder name to the path containing the mutant (mutant name is the same as the original file name)
    let mutationPaths = map (\n -> joinPath [mutantsDir, n, testFile ++ ".java"]) mutationFolders
    
    -- Calculate the wlp per method of all the mutants
    wlpMutants <- mapM (\path -> parseFile path >>= (\(env', decls', methods') -> wlpMethods env' decls' methods') >>= return . (path, )) mutationPaths
    
    -- A list containing a 1 or 0 per mutant, indicating the number of errors found
    errorsFound <- mapM (compareWlps env decls wlpOriginal) wlpMutants
    
    putStrLn ("Total number of mutants: " ++ show (length errorsFound))
    putStrLn ("Number of mutants in which we found an error: " ++ show (sum errorsFound))
    
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
    
-- | Calculates the wlp for every method in the source file. Also returns the type environment             
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
compareWlps :: TypeEnv -> [TypeDecl] -> [(Ident, Exp)] -> (String, [(Ident, Exp)]) -> IO Int
compareWlps env decls s (path, s') = do
    -- Get a list with for every method the number of errors found (1 or 0)
    errorsFound <- mapM compareMethod s
    -- Return 1 if we found at least 1 error
    return (if sum errorsFound > 0 then 1 else 0)
        where 
        compareMethod (ident, e) = case lookup ident s' of
                                    Nothing -> putStrLn ("The method \'" ++ show ident ++ "\' is missing in one of the mutations.") >> return 0 -- print a warning and return 0 errors found
                                    Just e' -> if unsafeIsTrue (extendEnv env decls ident) decls (e `imp` e') then return 0 else putStrLn ("error detected in mutation: " ++ path ++ " method: " ++ prettyPrint ident) >> return 1 -- print a message and return 1 error found

-- Gets the right post-condition given the type of a method
getPostCond :: Maybe Type -> Exp
getPostCond t = case parser Language.Java.Parser.exp postCond' of
                    Right e -> e
                    _       -> error "syntax error in post-condition"
    where postCond' = case t of
                        Nothing             -> postCondVoid
                        Just (RefType _)    -> postCondRefType
                        Just (PrimType _)   -> postCondPrimType
                    
-- Calculate the wlp (for testing purposes)
calcWlp :: IO ()
calcWlp = do
    source <- readFile (joinPath ["Equivalent mutations", "BST.java"]) -- sourcePath
    
    let result = parser compilationUnit source
    
    case result of
        Left error -> print error
        Right compUnit -> do
                            let decls = getDecls compUnit
                            let methods = if ignoreMainMethod then filter (/= Ident "main") (getMethodIds decls) else getMethodIds decls
                            let env = getTypeEnv compUnit decls methods
                            let printMethodWlp = (\ident e -> putStrLn (prettyPrint ident ++ ":\r\n" ++ prettyPrint e ++ "\r\n" ++ (if unsafeIsTrue (extendEnv env decls ident) decls e then "WLP evaluates to true" else (if unsafeIsFalse (extendEnv env decls ident) decls e then "WLP evaluates to false" else "undecidable")) ++ "\r\n"))
                            mapM_ (\ident -> wlpMethod env decls ident >>= printMethodWlp ident) methods
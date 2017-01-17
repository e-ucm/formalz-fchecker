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
    -- Get the wlp per method of the original source code
    (env, wlpOriginal) <- wlpMethods sourcePath
    
    -- Get the names of the folders containing the mutants (MAJOR creates a folder for every mutant)
    mutationFolders <- listDirectory mutantsDir
    
    -- Map each folder name to the path containing the mutant (mutant name is the same as the original file name)
    let mutationPaths = map (\n -> joinPath [mutantsDir, n, testFile ++ ".java"]) mutationFolders
    
    -- Calculate the wlp per method of all the mutants
    wlpMutants <- mapM (\path -> wlpMethods path >>= return . (path, ) . snd) mutationPaths
    
    mapM_ (compareWlps env wlpOriginal) wlpMutants
    
-- | Calculates the wlp for every method in the source file. Also returns the type environment             
wlpMethods :: FilePath -> IO (TypeEnv, [(Ident, Exp)])
wlpMethods s = do
    -- Get the source code
    source <- readFile s
    
    -- Parse the source code
    case parser compilationUnit source of
        Left parseError -> error (show parseError)
        Right compUnit -> do
                            let decls = getDecls compUnit
                            let methods = getMethodIds decls
                            let env = getTypeEnv compUnit decls methods
                            
                            -- Calculate the wlp per method
                            r <- mapM (\ident -> wlpMethod compUnit env decls ident >>= return . (ident, )) methods
                            
                            return (env, r)
                            
-- | Calculates the wlp for a given method
wlpMethod :: CompilationUnit -> TypeEnv -> [TypeDecl] -> Ident -> IO Exp
wlpMethod compUnit env decls ident = do
    let pred = wlpWithEnv decls env (fromJust' "wlpMethod" $ getMethod decls ident) postCond'
    return pred
    
-- | Compares the wlp per method of a program S and mutation S' by verifying that (wlp (S, m) q => wlp (S', m) q) holds for every method m
compareWlps :: TypeEnv -> [(Ident, Exp)] -> (String, [(Ident, Exp)]) -> IO ()
compareWlps env s (path, s') = mapM_ compareMethod s where
    compareMethod (ident, e) = case lookup ident s' of
                                Nothing -> putStrLn ("The method \'" ++ show ident ++ "\' is missing in one of the mutations.")
                                Just e' -> if unsafeIsTrue env (e `imp` e') then return () else putStrLn ("error detected in mutation: " ++ path)

-- The post-condition (for testing)
postCond' :: Exp
postCond' = case parser Language.Java.Parser.exp postCond of
            Right e -> e
            _       -> error "syntax error in post-condition"
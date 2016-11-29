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
                            putStrLn "\r\n-----WLP-----"
                            let pred = wlpWithEnv (getDecls compUnit) (getStaticVars compUnit) (getStmt compUnit) postCond'
                            putStrLn . prettyPrint $ pred
                            putStrLn "\r\n-----Correctness-----"
                            if unsafeIsTrue pred then putStrLn "WLP evaluates to true" else (if unsafeIsFalse pred then putStrLn "WLP evaluates to false" else putStrLn "undecidable")
                            
                            

-- Gets the statement(-block) defining the main method and initializes the heap
getStmt :: CompilationUnit -> Stmt
getStmt (CompilationUnit _ _ classTypeDecls) =  (head (concatMap getInit classTypeDecls)) where
    getInit (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = getInit' decls
    getInit' [] = []
    getInit' (MemberDecl (MethodDecl _ _ _ (Ident "main") _ _ (MethodBody (Just b))):_) = [StmtBlock b]
    getInit' (_:decls) = getInit' decls

-- Gets the class declarations
getDecls :: CompilationUnit -> [TypeDecl]
getDecls (CompilationUnit _ _ classTypeDecls) = classTypeDecls

-- Gets the static member declarations and puts them in the type environment
getStaticVars :: CompilationUnit -> TypeEnv
getStaticVars compUnit = concatMap fromTypeDecls (getDecls compUnit) where
    fromTypeDecls (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = concatMap fromMemberDecl decls
    fromMemberDecl (MemberDecl (FieldDecl mods t varDecls)) = if Static `elem` mods then map (fromVarDecl t) varDecls else []
    fromMemberDecl _                                        = []
    fromVarDecl t (VarDecl varId _) = (Name [getId varId], t)

-- The post-condition (for testing)
postCond' :: Exp
postCond' = case parser Language.Java.Parser.exp postCond of
            Right e -> e
            _       -> error "syntax error in post-condition"
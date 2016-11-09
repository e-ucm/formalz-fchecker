module Main where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Data.List

import WLP
import Verifier
import HelperFunctions



testFile = "objects"



main :: IO ()
main = do
    source <- readFile ("Tests\\" ++ testFile ++ ".java")
    
    
    let result = parser compilationUnit source
    
        
    putStrLn "-----Code-----"
    case result of
        Left error -> print error
        Right compUnit -> print compUnit --(getStmt compUnit)
    
    
    case result of
        Left error -> print error
        Right compUnit -> do
                            putStrLn "\r\n-----WLP-----"
                            let pred = wlpWithEnv (getDecls compUnit) (getStaticVars compUnit) (getStmt compUnit) postCond
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
postCond :: Exp
postCond = BinOp (ExpName (Name [Ident "x"])) Equal (Lit (Float 2))
module Main where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty

import WLP
import Verifier



testFile = "try-catch-finally"



main :: IO ()
main = do
    source <- readFile ("Tests\\" ++ testFile ++ ".java")
    
    
    let result = parser compilationUnit source
    
        
    putStrLn "-----Code-----"
    case result of
        Left error -> print error
        Right compUnit -> print compUnit --(getStmt compUnit)
    
    putStrLn "\r\n-----WLP-----"
    case result of
        Left error -> print error
        Right compUnit -> putStrLn . prettyPrint $ wlp (getStmt compUnit) postCond
        
        
    putStrLn "\r\n-----Correctness-----"
    case result of
        Left error -> print error
        Right compUnit -> if unsafeIsTrue (wlp (getStmt compUnit) postCond) then putStrLn "WLP evaluates to true" else (if unsafeIsFalse (wlp (getStmt compUnit) postCond) then putStrLn "WLP evaluates to false" else putStrLn "undecidable")
    

-- Gets the statement(-block) defining the main method for simple tests
getStmt :: CompilationUnit -> Stmt
getStmt (CompilationUnit _ _ classTypeDecls) = head (concatMap getInit classTypeDecls) where
    getInit (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = getInit' decls
    getInit' [] = []
    getInit' (MemberDecl (MethodDecl _ _ _ (Ident "main") _ _ (MethodBody (Just b))):_) = [StmtBlock b]
    getInit' (_:decls) = getInit' decls


-- The post-condition (for testing)
postCond :: Exp
postCond = BinOp (ExpName (Name [Ident "x"])) Equal (Lit (Int 1))
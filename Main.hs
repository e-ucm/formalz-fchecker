module Main where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser

import WLP





import Language.Java.Pretty


main :: IO ()
main = do
    source <- readFile "A:\\Thesis\\Test1.java"
    
    
    let result = parser compilationUnit source
    
    
    putStrLn "-----WLP-----"
    case result of
        Left error -> print error
        Right compUnit -> putStrLn . prettyPrint $ wlp (getStmt compUnit) postCond
        
    putStrLn "-----Code-----"
    case result of
        Left error -> print error
        Right compUnit -> print (getStmt compUnit)
    

-- Gets the statement(-block) defining the main method for simple tests
getStmt :: CompilationUnit -> Stmt
getStmt (CompilationUnit _ _ [ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))]) = getInit decls where
    getInit [] = error "No main method"
    getInit [MemberDecl (MethodDecl _ _ _ (Ident "main") _ _ (MethodBody (Just b)))] = StmtBlock b
    getInit (_:decls) = getInit decls


-- The post-condition (for testing)
postCond :: Exp
postCond = BinOp (ExpName (Name [Ident "x"])) Equal (Lit (Int 1))
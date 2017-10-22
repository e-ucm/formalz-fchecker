module Javawlp.SimpleFormulaChecker where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Z3.Monad
import Z3.Opts
import Data.Maybe

import Javawlp.Engine.Types
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

import LogicIR.Expr
import LogicIR.Frontend.Java
import LogicIR.Backend.Z3
import LogicIR.Backend.Pretty

import Control.Monad.Trans (liftIO)

import Debug.Trace

-- See README.md for more details.

type MethodDef = ([TypeDecl], Stmt, TypeEnv)

-- Takes a java source file and a method name and returns the class declarations,
-- the method body and the method's formal parameters.
parseMethod :: (FilePath, String) -> IO MethodDef
parseMethod (src, name) = do
    -- parse the Java source file:
    compilationnUnit <- parseJava src
    -- get all the class declarations in the Java source file; usually a single file defines only
    -- one class, but it could theoretically have more:
    let decls = getDecls compilationnUnit
    -- get the method's body ; to make it simple, the method's name is assumed to uniquely identify its body
    let mbody = fromJust $ getMethod decls (Ident name)
    -- get the method's formal parameters:
    let env = getMethodTypeEnv decls (Ident name)
    -- return the relevant data
    return (decls, mbody, env)
    where
    -- parse a Java source file, and extracts the necessary information from the compilation unit
    parseJava :: FilePath -> IO CompilationUnit
    parseJava s = do
        -- Get the source code
        source <- readFile s
        -- Parse the source code
        case parser compilationUnit source of
            Left parseError -> error (show parseError)
            Right compUnit  -> return compUnit

-- Get a list of all calls to a method of a specific name from a method definition.
getMethodCalls :: MethodDef -> String -> [MethodInvocation]
getMethodCalls (_, StmtBlock (Block bs), _) name = catMaybes (map extractMethodInv bs)
    where
        extractMethodInv :: BlockStmt -> Maybe MethodInvocation
        extractMethodInv (BlockStmt (ExpStmt (MethodInv i@(MethodCall (Name [Ident n]) _)))) = if n == name then Just i else Nothing
        extractMethodInv _ = Nothing

-- [pre(a), pre(b), pre(c)] -> (a AND b AND c)
extractExpr :: [MethodInvocation] -> Exp
extractExpr call = combineExprs $ map (\(MethodCall (Name [Ident _]) [a]) -> a) call
    where combineExprs :: [Exp] -> Exp
          combineExprs (e:[]) = e
          combineExprs (e:es) = BinOp e CAnd (combineExprs es)

-- Check if two Z3 AST's are equivalent
isEquivalent :: Z3 AST -> Z3 AST -> IO (Result, Maybe Model)
isEquivalent ast1' ast2' = evalZ3 z3
    where
    z3 = do
         ast1 <- ast1'
         ast2 <- ast2'
         astEq <- mkEq ast1 ast2
         astNeq <- mkNot astEq -- negate the question to get a model
         assert astNeq
         r <- solverCheckAndGetModel -- check in documentatie
         solverReset
         return r

-- Determine the equality of two method's pre/post conditions.
determineFormulaEq :: MethodDef -> MethodDef -> String -> IO ()
determineFormulaEq m1@(decls1, mbody1, env1) m2@(decls2, mbody2, env2) name = do
    -- get pre/post condition
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    let (lexpr1, lexpr2) = (javaExpToLExpr e1 env1 decls1, javaExpToLExpr e2 env2 decls2)
    let (ast1, ast2) = (lExprToZ3Ast lexpr1, lExprToZ3Ast lexpr2)
    putStrLn $ "e1:\n" ++ prettyPrint e1 ++ "\n\ne2:\n" ++ prettyPrint e2 ++ "\n"
    putStrLn $ "LogicIR.Expr 1:\n" ++ show lexpr1 ++ "\n\nLogicIR.Expr 2:\n" ++ show lexpr2 ++ "\n"
    putStrLn $ "LogicIR.Pretty 1:\n" ++ prettyLExpr lexpr1 ++ "\n\nLogicIR.Pretty 2:\n" ++ prettyLExpr lexpr2 ++ "\n"
    ast1s <- showZ3AST ast1
    putStrLn $ "Z3 AST 1:\n" ++ ast1s ++ "\n"
    ast2s <- showZ3AST ast2
    putStrLn $ "Z3 AST 2:\n" ++ ast2s ++ "\n"
    putStrLn "Z3 Result:"
    -- Check if the formula is satisfiable. If it is, print the instantiation of its free
    -- variables that would make it true:
    (result, model) <- isEquivalent ast1 ast2
    case result of
       Unsat -> putStrLn "formulas are equivalent!"
       Undef -> putStrLn "unable to decide the satisfiablity (TODO: use QuickCheck)"
       Sat   -> do
                putStrLn "formulas are NOT equivalent, model:"
                case model of
                  Just m -> do s <- evalZ3 (modelToString m)
                               putStr s
                  _      -> return ()
    where
        extractCond :: MethodDef -> String -> Exp
        extractCond m n = extractExpr (getMethodCalls m n)
        showZ3AST :: Z3 AST -> IO String
        showZ3AST ast' = evalZ3 $ ast' >>= astToString

-- Function that compares both the pre and the post condition for two methods.
-- It is assumed that both methods have the same environment (parameter names, class member names, etc).
compareSpec :: (FilePath, String) -> (FilePath, String) -> IO ()
compareSpec method1@(_, name1) method2@(_, name2) = do
    -- load the methods
    m1@(decls1, mbody1, env1) <- parseMethod method1
    m2@(decls2, mbody2, env2) <- parseMethod method2
    if env1 /= env2 then fail "inconsistent method parameters" else return ()
    if decls1 /= decls2 then fail "inconsistent class declarations (TODO)" else return ()
    putStrLn $ "----PRE---- (" ++ name1 ++ " vs " ++ name2 ++ ")"
    determineFormulaEq m1 m2 "pre"
    putStrLn "\n----POST---"
    determineFormulaEq m1 m2 "post"

edslSrc = "javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"
testEq = compareSpec (edslSrc, "swap_spec1") (edslSrc, "swap_spec1")
testNeq = compareSpec (edslSrc, "swap_spec1") (edslSrc, "swap_spec2")
blub = compareSpec (edslSrc, "getMax_spec1") (edslSrc, "getMax_spec2")
blub2 = compareSpec (edslSrc, "test1") (edslSrc, "test2")
blob = compareSpec (edslSrc, "blob1") (edslSrc, "blob1")
nullTest = compareSpec (edslSrc, "null1") (edslSrc, "null2")
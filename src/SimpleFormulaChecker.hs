module Javawlp.SimpleFormulaChecker where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Z3.Monad
import Data.Maybe

import Javawlp.Engine.Types
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

import LogicIR.Expr
import LogicIR.Frontend.Java
import LogicIR.Backend.Z3
import LogicIR.Backend.Pretty

import Debug.Trace

{-

Hi Duncan, I have written this simple Haskell program that shows you how to use some parts of this
Javawlp library for your purpose. The function "checkformula" below can be used to parse
a Java class of the form:

    class C {
       boolean f1(x,y,z) { return e1 ; }
       boolean f2(...  ) { return e2 ; }
       ...
    }

There is an example of such a class in "examples/DuncanExample.java". For e demo, call:

     checkformula "../examples/DuncanExample.java" "f1"
     checkformula "../examples/DuncanExample.java" "f2"
     checkformula "../examples/DuncanExample.java" "f3"

Each time, it will grab the formula in "return e" of the specified method, and will pass it to
Z3 to be checked if the formula is satisfiable. If it is, satisfying instances of the formula's free
variables will be printed.

The starting point to see of to ask Z3 to check Java's formula is the fuunction "unsafeIsSatisfiable"
in the module Verifier. This in turn calls "foldExp expAssertAlgebra expr" to translate expr to
an representation for Z3.

Some notes on the demo:

  ** To simplify the demo, the method-names must uniquely identify them.

  ** The used Java-parser library is a 3rd party. Unfortunately it is not that sophisticated.
     It seems to be blind to Java's operators' priorities.
     To parse e.g. x<0 || x>0 we have to explicityly brackets the sub-expressions: (x<0) || (x>0).


Dependencies:

   * Haskell package: language-java
   * Haskell package: z3, which then also need z3 binary/lib

-}

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
    -- | Parse a Java source file, and extracts the necessary information from the compilation unit
    parseJava :: FilePath -> IO CompilationUnit
    parseJava s = do
        -- Get the source code
        source <- readFile s
        -- Parse the source code
        case parser compilationUnit source of
            Left parseError -> error (show parseError)
            Right compUnit  -> return compUnit

getMethodCalls :: MethodDef -> String -> [MethodInvocation]
getMethodCalls (_, StmtBlock (Block bs), _) name = catMaybes (map extractMethodInv bs)
    where
        extractMethodInv :: BlockStmt -> Maybe MethodInvocation
        extractMethodInv (BlockStmt (ExpStmt (MethodInv i@(MethodCall (Name [Ident n]) _)))) = if n == name then Just i else Nothing
        extractMethodInv _ = Nothing

combineExprs :: Op -> [Exp] -> Exp
combineExprs o (e:[]) = e
combineExprs o (e:es) = BinOp e o (combineExprs o es)

andExprs :: [Exp] -> Exp
andExprs = combineExprs CAnd

orExprs :: [Exp] -> Exp
orExprs = combineExprs COr

extractExpr :: Op -> [MethodInvocation] -> Exp
extractExpr op call = combineExprs op $ map (\(MethodCall (Name [Ident _]) [a]) -> a) call

determineFormulaEq :: MethodDef -> MethodDef -> String -> IO ()
determineFormulaEq m1@(decls1, mbody1, env1) m2@(decls2, mbody2, env2) name = do
    -- get preconditions
    let (e1, e2) = (extractCond m1 name, extractCond m2 name)
    putStrLn $ "e1:\n" ++ prettyPrint e1 ++ "\n\ne2:\n" ++ prettyPrint e2 ++ "\n"
    let lexpr = javaExpToLExpr e1 env1 decls1
    putStrLn $ show lexpr ++ "\n\n" ++ prettyLExpr lexpr
    {--- get postconditions
    let (post1, post2) = (extractCond m1 "post", extractCond m2 "post")
    putStrLn $ "post1:\n" ++ prettyPrint post1 ++ "\npost2:\n" ++ prettyPrint post2 ++ "\n"-}
    -- Check if the formula is satisfiable. If it is, print the instantiation of its free
    -- variables that would make it true:
    (result,model) <- isEquivalent (env1, decls1, e1) (env2, decls2, e2)
    case result of
       Unsat -> putStrLn "formulas are equivalent!"
       Undef -> putStrLn "unable to decide the satisfiablity (TODO: use QuickCheck)"
       _     -> do
                putStrLn "formulas are NOT equivalent, model:"
                case model of
                  Just m -> do s <- evalZ3 (modelToString m)
                               putStr s
                  _      -> return ()
    where
        extractCond :: MethodDef -> String -> Exp
        extractCond m n = extractExpr CAnd (getMethodCalls m n)

compareSpec :: (FilePath, String) -> (FilePath, String) -> IO ()
compareSpec method1 method2 = do
    -- load the methods
    m1@(decls1, mbody1, env1) <- parseMethod method1
    m2@(decls2, mbody2, env2) <- parseMethod method2
    if env1 /= env2 then fail "inconsistent method parameters" else return ()
    if decls1 /= decls2 then fail "inconsistent class declarations (TODO)" else return ()
    putStrLn "----PRE----"
    determineFormulaEq m1 m2 "pre"
    putStrLn "\n----POST---"
    determineFormulaEq m1 m2 "post"

edslSrc = "javawlp_edsl/src/nl/uu/javawlp_edsl/Main.java"
testEq = compareSpec (edslSrc, "swap_spec1") (edslSrc, "swap_spec1")
testNeq = compareSpec (edslSrc, "swap_spec1") (edslSrc, "swap_spec2")

blub = compareSpec (edslSrc, "simple2") (edslSrc, "simple2")
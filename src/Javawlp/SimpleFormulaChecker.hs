module Javawlp.SimpleFormulaChecker where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Z3.Monad
import Data.Maybe

import Javawlp.Engine.Types
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

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

        
checkformula javasrc methodname = do
    -- parse the Java source file:
    compilationnUnit <- parseJava javasrc   
    -- get all the class declarations in the Java source file; usually a single file defines only
    -- one class, but it could theoretically have more:
    let decls = getDecls compilationnUnit
    
    -- get the method's body ; to make it simple, the method's name is assumed to uniquely identify its body
    let mbody = fromJust $ getMethod decls (Ident methodname)
    -- get the method's formal parameters:
    let env = getMethodTypeEnv decls  (Ident methodname)
    
    -- In this example, we expect the method's body to be of the form { return e ; }
    -- Let's grab e:
    let StmtBlock (Block [BlockStmt (Return (Just e))]) = mbody -- trace (show mbody) $ mbody
    
    let formula = e
    
    putStrLn ("\n** Formula to check: " ++ prettyPrint formula ++ "\n")
    
    -- Check if the formula is satisfiable. If it is, print the instantiation of its free
    -- variables that would make it true:
    let (result,model) = unsafeIsSatisfiable env decls formula
    case result of
       Unsat -> putStrLn "** UNSATISFIABLE."
       Undef -> putStrLn "** Unable to decide the satisfiablity."
       _     -> do
                putStrLn "** SATISFIABLE."
                case model of
                  Just m -> do { putStrLn "** Model:" ; s <- evalZ3 (modelToString m) ; putStrLn s }
                  _      -> return ()

    
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
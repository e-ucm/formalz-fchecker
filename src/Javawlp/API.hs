-- Copyright (c) 2017 Utrecht University
-- Author: Koen Wermer, Wishnu Prasetya

module Javawlp.API where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty
import Z3.Monad

import Javawlp.Engine.WLP
import Javawlp.Engine.Types
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

import Control.DeepSeq

-- | Parses a Java source file, and extracts the necessary information from the compilation unit
parseJava :: FilePath -> IO (TypeEnv, [TypeDecl], [Ident])
parseJava s = do
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
                            
                            
-- | Parse a string containing a Java-expression to a parse tree. This is intended to formulate
-- a post-condition to be passed to the wlp transformer.
post_ :: String -> Exp
post_ e = case parser Language.Java.Parser.exp e of
          Right e_ -> e_
          _        -> error "syntax error in post-condition"
                                                    
-- | Calculates the wlp for a given method. Example usage:
--
--     do (tyenv,decls,methods) <- parseJava filepath
--        configuration <- defaultConf
--        p <- wlpMethod configuration tyenv decls (Ident methodname) (post_ "returnValue == 0")
--        putStrLn (prettyPrint p)
--
wlpMethod :: WLPConf -> TypeEnv -> [TypeDecl] -> Ident -> Exp -> IO Exp
wlpMethod conf env decls methodName postCondition = do
    -- Find the return type of the method
    let returnType = getMethodType decls methodName -- for now, ignored
    
    -- Add returnValue to the type environment with the correct type
    let env' = extendEnv env decls methodName
    
    let methodBody = case getMethod decls methodName of
                     Just stmt -> stmt
                     _         -> error "wlpMethod: cannot get the method body."
    
    -- reset the counters for assigning fresh names    
    dummy1 <- resetVarPointer       
    dummy2 <- resetVarMethodInvokesCount
    -- Calculate the wlp:
    dummy1 `deepseq` (length dummy2) `deepseq` return $ wlpWithEnv conf decls env' methodBody postCondition
    
-- | A variant of wlpMethod where we can specify a list of post-conditions.
-- It returns a list of pairs (p,q) where q is a post-condition and p is the
-- calcuated wlp of q.    
wlpMethod_ :: WLPConf -> TypeEnv -> [TypeDecl] -> Ident -> [Exp] -> IO [(Exp,Exp)]    
wlpMethod_ conf env decls methodName postconds = sequence [wlp q | q <- postconds]
    where
    wlp q = do
            p <- wlpMethod conf env decls methodName q
            return (p,q)
     
       
-- | Calculates the wlps of a list of methods
wlpMethods :: WLPConf -> TypeEnv -> [TypeDecl] -> [(Ident,Exp)] -> IO [(Ident, Exp)]
wlpMethods conf env decls methods_and_postconds 
    = 
    sequence $ map (\(mname,postc) -> do { p <- wlpMethod conf env decls mname postc ; return (mname,p) }) methods'
    where
    methods' = if ignoreMainMethod conf 
               then filter (\(mname,_) -> mname /= Ident "main") methods_and_postconds  
               else methods_and_postconds    
   
-- | Variation of wlpMethods that takes a list of (m,postconditions)  
wlpMethods_ :: WLPConf -> TypeEnv -> [TypeDecl] -> [(Ident,[Exp])] -> IO [(Ident, [(Exp,Exp)])]
wlpMethods_ conf env decls methods_and_postconds 
    = 
    sequence $ map (\(mname,postcs) -> do { ps <- wlpMethod_ conf env decls mname postcs ; return (mname,ps) }) methods'
    where
    methods' = if ignoreMainMethod conf 
               then filter (\(mname,_) -> mname /= Ident "main") methods_and_postconds  
               else methods_and_postconds  

   
defaultConf :: WLPConf
defaultConf = WLPConf {
      nrOfUnroll=1,
      -- ignoreLibMethods=False,
      ignoreLibMethods=True,
      ignoreMainMethod =False
   }

-- | Print the wlp of a method with respect to a given post-condition.
printWlp :: String -> String -> String -> IO ()
printWlp sourcePath methodName postCond = do
  (typeEnv,decls,methodNames) <- parseJava sourcePath 
  let q = post_ postCond
  p <- wlpMethod defaultConf typeEnv decls (Ident methodName) q
  putStrLn $ showMethodWlp methodName q p
  putStrLn ("** Expr complexity of the wlp: " ++ show (exprComplexity p))
  --putStrLn ("\n### " ++ show p) 
  let (result,model) = unsafeIsSatisfiable (extendEnv typeEnv decls (Ident methodName)) decls p
  case result of
     Unsat -> putStrLn "** The wlp is UNSATISFIABLE."
     Undef -> putStrLn "** Unable to decide the wlp's satisfiablity."
     _     -> do
              putStrLn "** The wlp is SATISFIABLE."
              case model of
                Just m -> do { putStrLn "** Model:" ; s <- evalZ3 (modelToString m) ; putStrLn s }
                _      -> return ()
         

showMethodWlp :: String -> Exp -> Exp -> String
showMethodWlp methodName postCond wlp = 
       "** wlp of " ++ methodName ++ " over " ++ prettyPrint postCond
                 ++ " is "
                 ++ prettyPrint wlp
                 
                 
                 


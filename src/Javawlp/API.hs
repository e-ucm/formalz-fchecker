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

import Debug.Trace
-- import Control.DeepSeq

-- | Parses a Java source file, and extracts the necessary information from the compilation unit
parseJava :: FilePath -> IO CompilationUnit
parseJava s = do
    -- Get the source code
    source <- readFile s
    
    -- Parse the source code
    case parser compilationUnit source of
        Left parseError -> error (show parseError)
        Right compUnit  -> return compUnit
           
-- | Parse a string containing a Java-expression to a parse tree. This is intended to formulate
-- a post-condition to be passed to the wlp transformer.
post_ :: String -> Exp
post_ e = case parser Language.Java.Parser.exp e of
          Right e_ -> e_
          _        -> error "syntax error in post-condition"
                                                    
-- | Calculates the wlp for a given method. Example usage:
--
--     do compunit <- parseJava filepath
--        configuration <- defaultConf
--        p <- wlpMethod configuration compunit methodname (post_ "returnValue == 0")
--        putStrLn (prettyPrint p)
--
wlpMethod :: WLPConf -> CompilationUnit -> String -> Exp -> IO Exp
wlpMethod conf compUnit methodName postCondition = do
    -- Find the return type of the method
    -- let returnType = getMethodType decls methodName -- for now, ignored
    let decls = getDecls compUnit
    let env = getTypeEnv compUnit decls [Ident methodName]
    -- Add returnValue to the type environment with the correct type
    let env' = extendEnv env decls (Ident methodName)

    let methodBody = case getMethod decls (Ident methodName) of
                     Just stmt -> stmt
                     _         -> error "wlpMethod: cannot get the method body."
    
    -- reset the counters for assigning fresh names    
    -- dummy1 <- resetVarPointer       
    -- dummy2 <- resetVarMethodInvokesCount
    
    -- We need to resent the method-invocation counters to make generated names the same accross multiple
    -- invocation of the wlp over the same method. However the above resets do not really work, due to
    -- the combination of lazy-evaluation and unsafeIO that screw things up. The above resets are not
    -- guaranteed to be executed. Messy. So instead, we enforce renumbering by applying post-processing
    -- normalization:
    
    -- Calculate the wlp:
    let p = wlpWithEnv conf decls (trace ("\n##\n" ++ prettyprintTypeEnv env') env') methodBody postCondition
    -- return p
    return $ normalizeInvocationNumbers p
    
    
    
    
-- | A variant of wlpMethod where we can specify a list of post-conditions.
-- It returns a list of pairs (p,q) where q is a post-condition and p is the
-- calcuated wlp of q.    
wlpMethod_ :: WLPConf -> CompilationUnit -> String -> [Exp] -> IO [(Exp,Exp)]    
wlpMethod_ conf compunit methodName postconds = sequence [wlp q | q <- postconds]
    where
    wlp q = do
            p <- wlpMethod conf compunit methodName q
            return (p,q)
     
       
-- | Calculates the wlps of a list of methods
wlpMethods :: WLPConf -> CompilationUnit -> [(String,Exp)] -> IO [(String, Exp)]
wlpMethods conf compunit methods_and_postconds 
    = 
    sequence $ map (\(mname,postc) -> do { p <- wlpMethod conf compunit mname postc ; return (mname,p) }) methods'
    where
    methods' = if ignoreMainMethod conf 
               then filter (\(mname,_) -> mname /= "main") methods_and_postconds  
               else methods_and_postconds    
   
-- | Variation of wlpMethods that takes a list of (m,postconditions)  
wlpMethods_ :: WLPConf -> CompilationUnit -> [(String,[Exp])] -> IO [(String, [(Exp,Exp)])]
wlpMethods_ conf compunit methods_and_postconds 
    = 
    sequence $ map (\(mname,postcs) -> do { ps <- wlpMethod_ conf compunit mname postcs ; return (mname,ps) }) methods'
    where
    methods' = if ignoreMainMethod conf 
               then filter (\(mname,_) -> mname /= "main") methods_and_postconds  
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
  compunit <- parseJava sourcePath 
  let decls = getDecls compunit
  let env = getTypeEnv compunit decls [Ident methodName]
  let env' = extendEnv env decls (Ident methodName)
  let q = post_ postCond
  p <- wlpMethod defaultConf compunit methodName q
  putStrLn $ showMethodWlp methodName q p
  putStrLn ("** Expr complexity of the wlp: " ++ show (exprComplexity p))
  --putStrLn ("\n### " ++ show p) 
  let (result,model) = unsafeIsSatisfiable env' decls p
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
                 
                 
                 


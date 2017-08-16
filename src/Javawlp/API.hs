-- Copyright (c) 2017 Utrecht University
-- Author: Koen Wermer, Wishnu Prasetya

module Javawlp.API where

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty

import Javawlp.Engine.WLP
import Javawlp.Engine.Types
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

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
                                                    
-- | Calculates the wlp for a given method
wlpMethod :: WLPConf -> TypeEnv -> [TypeDecl] -> Ident -> Exp -> IO Exp
wlpMethod conf env decls methodName postCondition = do
    -- Find the return type of the method
    let returnType = getMethodType decls methodName -- for now, ignored
    
    -- Add returnValue to the type environment with the correct type
    let env' = extendEnv env decls methodName
    
    let methodBody = case getMethod decls methodName of
                     Just stmt -> stmt
                     _         -> error "wlpMethod: cannot get the method body."
                    
    -- Calculate the wlp:
    return $ wlpWithEnv conf decls env' methodBody postCondition
   
-- | Calculates the wlps of a list of methods
wlpMethods :: WLPConf -> TypeEnv -> [TypeDecl] -> [(Ident,Exp)] -> IO [(Ident, Exp)]
wlpMethods conf env decls methods_and_postconds 
    = 
    sequence $ map (\(mname,pcond) -> do { q <- wlpMethod conf env decls mname pcond ; return (mname,q) }) methods'
    where
    methods' = if ignoreMainMethod conf 
               then filter (\(mname,_) -> mname /= Ident "main") methods_and_postconds  
               else methods_and_postconds    
   
   
defaultConf :: WLPConf
defaultConf = WLPConf {
      nrOfUnroll=1,
      ignoreLibMethods=False,
      ignoreMainMethod =False
   }

-- | Print the wlp of a method with respect to a given post-condition.
printWlp :: String -> String -> String -> IO ()
printWlp sourcePath methodName postCond = do
  (typeEnv,decls,methodNames) <- parseJava sourcePath 
  let q = post_ postCond
  p <- wlpMethod defaultConf typeEnv decls (Ident methodName) q
  putStrLn $ showMethodWlp methodName q p

showMethodWlp :: String -> Exp -> Exp -> String
showMethodWlp methodName postCond wlp = 
       "wlp of " ++ methodName ++ " over " ++ prettyPrint postCond
                 ++ " is "
                 ++ prettyPrint wlp

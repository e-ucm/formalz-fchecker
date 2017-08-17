-- Contain various functions to automate the experiment
module Experiment where
    
import Javawlp.API 
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier

import Language.Java.Syntax
import Z3.Monad



checkMutant original mutantDir methodName postcond = do 
    let srcName = original ++ ".java"
    (typeEnv1,decls1,methodNames1) <- parseJava ("./subjects/src/" ++ srcName) 
    (typeEnv2,decls2,methodNames2) <- parseJava (mutantDir ++ "/"  ++ srcName) 
    
    let mname = Ident methodName
    let q = post_ postcond
    p1 <- wlpMethod defaultConf typeEnv1 decls1 mname q
    p2 <- wlpMethod defaultConf typeEnv2 decls2 mname q
    let tocheck    = PreNot (p1 ==* p2)
    let (result,_) = unsafeIsSatisfiable (extendEnv typeEnv1 decls1 mname) decls1 tocheck
    case result of
       Sat   -> putStrLn "** KILLED."
       Unsat -> putStrLn "** SURVIVED."
       _     -> putStrLn "** UNDECIDED."

         
    
-- Contain various functions to automate the experiment
module Experiment where
    
import Javawlp.API 
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier
import Javawlp.Engine.WLP

import Language.Java.Syntax
import Z3.Monad
import Data.List
import System.IO.Unsafe
import System.FilePath
import System.Directory

-- specify the wlp global configuration here
wlpConfiguretion = WLPConf {
      nrOfUnroll=1,
      ignoreLibMethods=False,
      ignoreMainMethod =False
   }

-- Calculate the wlps of an orginal java method and its mutant, then compare them.
-- Example: 
--
--    checkMutant Triangle mutantdir tritype1 ["returnValue==0", "returnValue==1"]
--
-- This will check the method tritype1 in subjects/src/Triangle.java vs mdir/Triangle.java
--
checkMutant :: String -> FilePath  -> String -> [String] -> IO (Maybe Bool)
checkMutant original mutantDir methodName postconds = do 
    let srcName = original ++ ".java"
    let srcPath    = "./subjects/src" </>  srcName
    let mutantPath = mutantDir </> srcName
    (typeEnv1,decls1,methodNames1) <- parseJava srcPath
    (typeEnv2,decls2,methodNames2) <- parseJava mutantPath 
    
    let mname = Ident methodName
    let qs = [ post_ q | q <- postconds ]
    -- this function will be used to compare the resulting wlps
    let comparePreC p1 p2 = let 
                            f = PreNot (p1 ==* p2)
                            (result,_) = unsafeIsSatisfiable (extendEnv typeEnv1 decls1 mname) decls1 f
                            in
                            result
                            
    ps1 <- sequence [ wlpMethod wlpConfiguretion typeEnv1 decls1 mname q | q <- qs ]
    ps2 <- sequence [ wlpMethod wlpConfiguretion typeEnv2 decls2 mname q | q <- qs ]
    putStrLn ("## Checking mutant " ++ mutantPath)
    let z = zipWith comparePreC ps1 ps2
    if any (== Sat) z        then return $ Just False -- the mutant is killed
    else if all (== Unsat) z then return $ Just True  -- the mutant definitely survives
    else return Nothing                          -- some of the wlps are undecidable
    
  
         
checkAllMutants original mutantsRootDir methodName postconds = do   
    mdirs <- getMutantsSubdirs mutantsRootDir
    let mdirs_ = [ mutantsRootDir </> d | d <- mdirs ]
    results <- sequence [ checkMutant original md methodName postconds | md <- mdirs_ ]
    let nmutants = length results
    let killed = length $ filter (== Just False) results
    let undecided = length $ filter (== Nothing) results
    putStrLn ("** Checking " ++ original ++ "." ++ methodName ++ " on " ++ show nmutants ++ " mutants.")
    putStrLn ("** Killed=" ++ show killed ++ ", Survived=" ++ show (nmutants - killed)
              ++ " (incl. " ++ show undecided ++ " undecided)" )
    let survivors = [ (m,r) | (m,r) <- zip mdirs results, not(r == Just False) ]
    sequence_ [ putStrLn ("** Unable to kill mutant " ++ m ++ ", " ++ show r) | (m,r) <- survivors ]
    where
    getMutantsSubdirs mutantsRootDir = do
         fs <- listDirectory mutantsRootDir
         let mdirs = [ name | name <- fs, not("." `isPrefixOf` name) 
                                          && unsafePerformIO (doesDirectoryExist (mutantsRootDir </> name))
                      ]
         return mdirs
    
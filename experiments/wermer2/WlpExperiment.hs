--
-- Contain the script to run the wlp experiment.
--
module WlpExperiment where
    
import Javawlp.API 
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier
import Javawlp.Engine.WLP

import Language.Java.Syntax
import Z3.Monad
import Data.List
import System.IO
import System.IO.Unsafe
import System.FilePath
import System.Directory

   
-- ======================================================================
-- The top-level main functions to run the wlp-experiment on one or all subjects.
-- ======================================================================
    
main = do 
    clean
    sequence_ [ main1 "FN" s | s <- subjects ]    -- checking wlp's false negatives rate, using Major's generated mutants
    sequence_ [ main1 "FP" s | s <- subjectsFP ]  -- checking wlp's false positives rate, using manually crafted equivalent mutants 
    
    
main1 mode subject = do 
      let (classname,targetMethodName,postconditions) = subject
      checkAllMutants mode classname targetMethodName postconditions
       
clean = do
   removeFile logFile
   removeFile dataFile
      
-- ======================================================================
-- List of all subjects: class name, target method name, list of post-conditions
-- ======================================================================

-- all subjects
subjects = [ ("Triangle", "tritype1", ["returnValue == -1", "returnValue == 0", "returnValue == 1", "returnValue == 2"]) ]  

-- subjects which are to be included for false-positive-checking experiment
subjectsFor_FP_experiment = [ ]
 
subjectsFP = [ s | s@(cn,fn,_) <- subjects, (cn ++ "." ++ fn) `elem` subjectsFor_FP_experiment ] 

-- ======================================================================
-- Configuration parameters
-- ======================================================================

subjectsDir = "." </> "subjects" </> "src" 
majorMutantsDir       = "." </> "mutants" </> "generated"
handcraftedMutantsDir = "." </> "mutants" </> "handcrafted"
dataDir     = "." </> "data"
tmpDir      = "." </> "tmp"

-- specify the wlp global configuration here
wlpConfiguretion = WLPConf {
      nrOfUnroll=1,
      ignoreLibMethods=False,
      ignoreMainMethod =False
   }

-- ======================================================================
-- Logging, debug, and writing to the output data-file
-- ======================================================================

logFile = tmpDir </> "wlplog.log"
logWrite s = do { debugWrite s ; appendFile logFile s }
logWriteLn s = logWrite (s ++ "\n")

debugWrite s = hPutStr stderr s
debugWriteLn s = debugWrite (s ++ "\n")

dataFile = dataDir </> "wlpResults.txt"
writeToDataFile items = appendFile dataFile $ (concat $ intersperse "," items) ++ "\n"

-- ======================================================================
-- Main functions to check a mutant vs orginal using wlp.
-- ======================================================================
    
-- Calculate the wlps of an orginal java method and its mutant, then compare them.
-- Example: 
--
--    rawCheckMutant Triangle mdir tritype1 ["returnValue==0", "returnValue==1"]
--
-- This will check the method tritype1 in subjects/src/Triangle.java vs mdir/Triangle.java
--
rawCheckMutant :: String -> FilePath  -> String -> [String] -> IO (Maybe Bool)
rawCheckMutant original mutantDir methodName postconds = do 
    let srcName = original ++ ".java"
    let srcPath    = subjectsDir </>  srcName
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
    logWriteLn ("## Checking mutant " ++ mutantPath)
    let z = zipWith comparePreC ps1 ps2
    if any (== Sat) z        then return $ Just False -- the mutant is killed
    else if all (== Unsat) z then return $ Just True  -- the mutant definitely survives
    else return Nothing                          -- some of the wlps are undecidable
    
-- check all mutants of a single subject
rawCheckAllMutants original mutantsRootDir methodName postconds = do   
    mdirs <- getMutantsSubdirs mutantsRootDir
    let mdirs_ = [ mutantsRootDir </> d | d <- mdirs ]
    results <- sequence [ rawCheckMutant original md methodName postconds | md <- mdirs_ ]
    let killed = length $ filter (== Just False) results
    debugWriteLn ("## " ++ original ++ "." ++ methodName ++ ", killed: " ++ show killed ++ "/" ++ show (length results))
    return (zip mdirs results)
    where
    getMutantsSubdirs mutantsRootDir = do
         fs <- listDirectory mutantsRootDir
         let mdirs = [ name | name <- fs, not("." `isPrefixOf` name) 
                                          && unsafePerformIO (doesDirectoryExist (mutantsRootDir </> name))
                      ]
         return mdirs
         
                  
checkAllMutants mode subjectClassName subjectMethodName postconds = do  
    
    let mutantsDir = if mode == "FN" 
                     then majorMutantsDir </> subjectClassName
                     else handcraftedMutantsDir </> subjectClassName
    
    results <- rawCheckAllMutants subjectClassName mutantsDir subjectMethodName postconds
     
    let nmutants = length results
    let killed = length $ filter (== Just False) $ map snd results
    let undecided = length $ filter (== Nothing) $ map snd results
    putStrLn ("** " ++ mode ++ " checking " ++ subjectClassName ++ "." ++ subjectMethodName ++ " on " ++ show nmutants ++ " mutants.")
    putStrLn ("** Killed=" ++ show killed ++ ", Survived=" ++ show (nmutants - killed)
              ++ " (incl. " ++ show undecided ++ " undecided)" )
    let survivors = [ (m,r) | (m,r) <- results, not(r == Just False) ]
    sequence_ [ putStrLn ("** Unable to kill mutant " ++ m ++ ", " ++ show r) | (m,r) <- survivors ]
    
    writeToDataFile $ [ mode,
                        subjectClassName ++ "." ++ subjectMethodName,
                        show nmutants,
                        show killed,
                        show (nmutants - killed) ]
                        ++
                        map fst survivors
    
    
    
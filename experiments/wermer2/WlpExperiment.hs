--
-- Contain the script to run the wlp experiment.
--
module WlpExperiment where
    
import Javawlp.API 
import Javawlp.Engine.HelperFunctions
import Javawlp.Engine.Verifier
import Javawlp.Engine.WLP
import Language.Java.Pretty

import Language.Java.Syntax
import Z3.Monad
import Data.List
import System.IO
import System.IO.Unsafe
import System.FilePath
import System.Directory
import System.CPUTime
import Data.IORef
import Control.DeepSeq
import Debug.Trace

   
-- ======================================================================
-- The top-level main functions to run the wlp-experiment on one or all subjects.
-- ======================================================================
    
main = do 
    clean
    sequence_ [ main1 s | s <- subjects ]  
    
main1 subject = do 
      let (classname,targetMethodName,postconditions) = subject
      let check configuration experimentName mutantDir postConds = checkAllMutants configuration experimentName classname targetMethodName mutantDir postConds
      let conf1 = stdWlpConfiguration
      -- this blows up, unfortunately:
      -- let conf2 = stdWlpConfiguration{nrOfUnroll=2}
      
      let mutantDir1 = majorMutantsDir </> classname
      let mutantDir2 = handcraftedMutantsDir </> classname
      
      check conf1 (classname ++ "_bug_trivpost")  mutantDir1 ["true"]
      check conf1 (classname ++ "_bug_simplepost")  mutantDir1 postconditions
      -- check conf2 (classname ++ "_bug_simplepost2") mutantDir1  postconditions
      
      if classname `elem` has_equivmutants
          then do
               -- check conf1 (classname ++ "_equiv_trivpost")  mutantDir2 ["true"]
               check conf1 (classname ++ "_equiv_simplepost")  mutantDir2  postconditions
               -- check conf2 (classname ++ "_equiv_simplepost2") mutantDir2  postconditions
          else return ()
      
       
clean = do
   removeFile logFile
      
-- ======================================================================
-- List of all subjects: class name, target method name, list of post-conditions
-- ======================================================================

-- All subjects
-- Each is specified as a tuple: (target-class, method-name, [post-cond1, post-cond2, ... ])
--
subjects = [ ("Triangle", "tritype1", ["returnValue == -1", "returnValue == 0", "returnValue == 1", "returnValue == 2"]) ,
             ("MinsMaxs", "getMinsMaxs", ["(mins[0]==0)" , "(maxs[0]==1)" , "(mins[1]==1)"]),
             ("Fibonacci", "fibonacciInteractive", ["f3==0"]) -- f3==0 is actually not a possibele output; but this will kill all major mutants; f3==2 on the otherhand, does not kill any mutant!
           ]  

-- specify which subjects has equivalent mutants to compare against
has_equivmutants = ["MinsMaxs","Fibonacci"]

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

-- standard wlp-configuration
stdWlpConfiguration = WLPConf {
      nrOfUnroll=1,
      ignoreLibMethods=True,
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
-- Returns an indication whether the mutant is killed, and the total complexity measure of formulas to check
--
rawCheckMutant :: WLPConf -> String -> FilePath  -> String -> [String] -> IO (Maybe Bool, Int)
rawCheckMutant wlpConfiguration original mutantDir methodName postconds = do 
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
                            (result,_) = unsafeIsSatisfiable (extendEnv typeEnv1 decls1 mname) decls1 $ trace ("### " ++ prettyPrint f) f
                            complexity = exprComplexity f
                            in
                            (result,complexity)
                            
    ps1 <- sequence [ wlpMethod wlpConfiguration typeEnv1 decls1 mname q  | q <- qs ]
    ps2 <- sequence [ wlpMethod wlpConfiguration typeEnv2 decls2 mname q  | q <- qs ]
    
    logWriteLn ("## Checking mutant " ++ mutantPath)
    let z_ = zipWith comparePreC ps1 ps2
    let z  = map fst z_
    -- total complexity ... we sum the complexities of all the conditions to check:
    let complexity = sum (map snd z_)
    if any (== Sat) z        then return (Just False,complexity) -- the mutant is killed
    else if all (== Unsat) z then return (Just True,complexity)  -- the mutant definitely survives
    else return (Nothing,complexity)                             -- some of the wlps are undecidable
    

    
-- check all mutants of a single subject
rawCheckAllMutants wlpConfiguration original mutantsRootDir methodName postconds = do   
    mdirs <- getMutantsSubdirs mutantsRootDir
    let mdirs_ = [ mutantsRootDir </> d | d <- mdirs ]
    results <- sequence [ rawCheckMutant wlpConfiguration original md methodName postconds | md <- mdirs_ ]
    let killed = length $ filter (== Just False) $ map fst results
    debugWriteLn ("## " ++ original ++ "." ++ methodName ++ ", killed: " ++ show killed ++ "/" ++ show (length results))
    return (zip mdirs results)
    where
    getMutantsSubdirs mutantsRootDir = do
         fs <- listDirectory mutantsRootDir
         let mdirs = [ name | name <- fs, not("." `isPrefixOf` name) 
                                          && unsafePerformIO (doesDirectoryExist (mutantsRootDir </> name))
                      ]
         return mdirs
         
                  
checkAllMutants wlpConfiguration 
                experimentName  -- will be used to prefix the name of the generated data file
                subjectClassName 
                subjectMethodName 
                mutantsDir
                postconds 
                = 
    do  
    putStrLn ("** Starting experiment " ++ experimentName ++ "...")
    time1   <- getCPUTime         
    results <- rawCheckAllMutants wlpConfiguration subjectClassName mutantsDir subjectMethodName postconds
    
    let killInfos = map fst $ map snd results
    let complexityInfos = map snd $ map snd results
    let maxFormulaComplexity = foldr max 0 complexityInfos
    
    let nmutants = length killInfos
    let killed = length $ filter (== Just False) killInfos
    let undecided = length $ filter (== Nothing) killInfos
    putStrLn ("** Experiment " ++ experimentName ++ ", checking " ++ subjectClassName ++ "." ++ subjectMethodName ++ " on " ++ show nmutants ++ " mutants.")
    putStrLn ("** Killed=" ++ show killed ++ ", Survived=" ++ show (nmutants - killed)
              ++ " (incl. " ++ show undecided ++ " undecided)" )
    time2 <- getCPUTime
    
    let runtime_duration = (time2 - time1) `div` (10^9) -- in milli seconds
    
    let survivors = [ (m,r) | (m,(r,_)) <- results, not(r == Just False) ]
    sequence_ [ putStrLn ("** Unable to kill mutant " ++ m ++ ", " ++ show r) | (m,r) <- survivors ]
    
    writeFile dataFile "Subject, number-of-mutants, #killed, #survivors, formula-complexity, runtime, list-of-survivors\n" 
    writeItemsToDataFile $
             [ subjectClassName ++ "." ++ subjectMethodName,
               show nmutants,
               show killed,
               show (nmutants - killed),
               show maxFormulaComplexity,
               show runtime_duration ]
               ++
               map fst survivors
    
    
    where

    dataFile = dataDir </> (experimentName ++ "_wlpResults.txt")
    writeItemsToDataFile items = appendFile dataFile $ (concat $ intersperse "," items) ++ "\n"
    
    
module Daikon where

import Control.Monad
import System.FilePath(joinPath)
import System.Directory

source, pathDir, packageDir, methodName :: String
source = "GradientFunction"
pathDir = "org/apache/commons/math3/analysis/differentiation"
methodName = "value"

packageDir = map (\c -> if c == '/' then '.' else c) pathDir

-- Creates a shell script that checks the daikon invariants against all mutations for the current test file
createDaikonScript :: IO ()
createDaikonScript = do
    writeFile scriptName ("#!/bin/sh\nHOME=\"T3Daikon\"\nSOURCE=\"" ++ source ++ "\"\nPATHDIR=\"" ++ pathDir ++ "\"\nPACKAGEDIR=\"" ++ packageDir ++ "\"\nMETHODNAME=\"" ++ methodName ++ "\"\n\n")
        
    mutationFolders <- listDirectory (joinPath ["..", source ++ " mutants"])
    checkMutants (length mutationFolders)
    
    appendFile scriptName "read -p \"Press enter to continue\""
    
    
scriptName :: FilePath
scriptName = joinPath["..", "Daikon check inv " ++ source ++ ".sh"]

-- Adds the code to check the mutants
checkMutants :: Int -> IO ()
checkMutants 0 = return ()
checkMutants n = do
    appendFile scriptName ("echo \"compiling mutation " ++ show n ++ "..\"\njavac -cp \".;wlp/Tests/\" -d \"wlp/Tests\" \"$SOURCE mutants/" ++ show n ++ "/$PATHDIR/$SOURCE.java\"\n\n")
    appendFile scriptName ("echo \"generating test suite\"\njava -cp \".;$HOME/KWT3daikon.jar;$HOME/libs/T3.jar;$HOME/libs/T3Daikon.jar;wlp/tests\" MyT3Daikon.Generator wlp/tests $PACKAGEDIR.$SOURCE $METHODNAME 50 1000 \"Daikon test suites/$SOURCE\"\n\n")
    appendFile scriptName ("echo \"checking invariants " ++ show n ++ "..\"\njava -cp \".;$HOME/KWT3daikon.jar;$HOME/libs/T3.jar;$HOME/libs/T3Daikon.jar;wlp/Tests\" MyT3Daikon.Miner -check \"Daikon test suites/$SOURCE.tr\" $METHODNAME \"Daikon invariants/$SOURCE\"\n\n")
    checkMutants (n-1)
-- Contains all code used for running the daikon experiments
module Daikon where

import Control.Monad
import System.FilePath(joinPath)
import System.Directory
import Data.List

testNr :: Int
testNr = 3

source, pathDir, packageDir, methodName :: String
source = case testNr of
            1 -> "Iterator"
            2 -> "GradientFunction"
            3 -> "BaseSecantSolver"
pathDir = case testNr of
            1 -> "org/apache/commons/math3/util"
            2 -> "org/apache/commons/math3/analysis/differentiation"
            3 -> "org/apache/commons/math3/analysis/solvers"
methodName = case testNr of
                1 -> "hasNext"
                2 -> "value"
                3 -> "verifyBracketing"

packageDir = map (\c -> if c == '/' then '.' else c) pathDir

-- Creates a shell script that checks the daikon invariants against all mutations for the current test file
createDaikonScript :: IO ()
createDaikonScript = do
    writeFile scriptName ("#!/bin/sh\nHOME=\"T3Daikon\"\nSOURCE=\"" ++ source ++ "\"\nPATHDIR=\"" ++ pathDir ++ "\"\nPACKAGEDIR=\"" ++ packageDir ++ "\"\nMETHODNAME=\"" ++ methodName ++ "\"\n\n")
        
    mutationFolders <- listDirectory (joinPath ["..", "daikon " ++ source ++ " mutants"])
    checkMutants (length mutationFolders)
    
    appendFile scriptName "read -p \"Press enter to continue\""
    
    
scriptName :: FilePath
scriptName = joinPath["..", "Daikon check inv " ++ source ++ " " ++ methodName ++ ".sh"]

-- Adds the code to check the mutants
checkMutants :: Int -> IO ()
checkMutants 0 = return ()
checkMutants n = do
    appendFile scriptName ("echo \"compiling mutation " ++ show n ++ "..\"\njavac -cp \".;wlp/Tests/\" -d \"wlp/Tests\" \"daikon $SOURCE mutants/" ++ show n ++ "/$PATHDIR/$SOURCE.java\"\n\n")
    appendFile scriptName ("echo \"checking invariants " ++ show n ++ "..\"\njava -cp \".;$HOME/KWT3daikon.jar;$HOME/libs/T3.jar;$HOME/libs/T3Daikon.jar;wlp/Tests\" MyT3Daikon.Miner -check \"Daikon test suites/$SOURCE.tr\" $METHODNAME \"Daikon invariants/$SOURCE\"\n\n")
    checkMutants (n-1)
    
    
-- Renames mutant directories by subtracting a value from the mutant number
renameMutants :: Int -> IO ()
renameMutants n = do
    let mutantsPath = joinPath ["..", "daikon " ++ source ++ " mutants"]
    mutationFolders <- listDirectory (mutantsPath)
    mapM_ (\old -> renameDirectory (joinPath [mutantsPath, show old]) (joinPath [mutantsPath, show (old - n)])) (sort (map read mutationFolders :: [Int]))
    
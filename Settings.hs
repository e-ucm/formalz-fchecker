module Settings where

import System.FilePath(joinPath)

testFile, postCondVoid, postCondRefType, postCondPrimType :: String
testFile = test3

-- The post condition may depend on the type of the method we are looking at
postCondVoid = "true"
postCondRefType = heur1
postCondPrimType = heur2

-- When ignoreLibMethods is true, library calls will simply be ignored. When false, we consider library methods but make no assumptions about them (so the WLP will be true)
-- To prevent insanely long calculation times, we may decide to not calculate the wlp of the main method when ignoring library methods
ignoreLibMethods, ignoreMainMethod :: Bool
ignoreLibMethods = False
ignoreMainMethod = False


nrOfUnroll :: Int
nrOfUnroll = 1

-- The classpath for test files
classPath :: FilePath -> FilePath
classPath "basesecantsolver" = joinPath ["org", "apache", "commons", "math3", "analysis", "solvers"]
classPath "gradientfunction" = joinPath ["org", "apache", "commons", "math3", "analysis", "differentiation"]
classPath "iterator" = joinPath ["org", "apache", "commons", "math3", "util"]
classPath _ = ""


-- Some constants, for convenience:
test1 = "basesecantsolver"
test2 = "gradientfunction"
test3 = "iterator"

heur0 = "true"
heur1 = "returnValue != null"
heur2 = "returnValueVar == returnValue"
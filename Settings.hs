module Settings where

testFile, postCondVoid, postCondRefType, postCondPrimType :: String
testFile = "arrays1"

-- The post condition may depend on the type of the method we are looking at
postCondVoid = "true"
postCondRefType = "returnValue != null"
postCondPrimType = "returnValue == returnValueVar"

-- When ignoreLibMethods is true, library calls will simply be ignored. When false, we consider library methods but make no assumptions about them (so the WLP will be true)
-- To prevent insanely long calculation times, we may decide to not calculate the wlp of the main method when ignoring library methods
ignoreLibMethods, ignoreMainMethod :: Bool
ignoreLibMethods = False
ignoreMainMethod = False


nrOfUnroll :: Int
nrOfUnroll = 1
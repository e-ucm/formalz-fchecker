module LogicIR.Backend.Z3.Pretty (showRelevantModel) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Z3.Monad
import Z3.Opts

import LogicIR.Backend.Z3.Model

-- | Function that shows a human-readable model and also highlights potential inconsistencies.
showRelevantModel :: Z3Model -> IO ()
showRelevantModel model = do
  putStrLn "Pretty model:"
  mapM_ (putStrLn . prettyModelVal) $ fromKeys (consts ++ arrays)
  where modelMap :: M.Map String ModelVal
        modelMap = M.fromList model
        modelClean :: M.Map String ModelVal
        modelClean = M.filterWithKey (\k _ -> '!' `notElem` k) $ M.map modelCleanFunc modelMap
        fromKeys :: [String] -> [(String, ModelVal)]
        fromKeys = map (\k -> let v = M.findWithDefault defaultArray k modelClean in (k, v))
        defaultArray :: ModelVal
        defaultArray = ArrayFunc [InstElse $ IntVal (-111)] -- nullTest2
        -- Pretty print the model value
        prettyModelVal :: (String, ModelVal) -> String
        prettyModelVal (k, BoolVal b) = k ++ " = " ++ if b then "true" else "false"
        prettyModelVal (k, IntVal n) = k ++ " = " ++ show n
        prettyModelVal (k, RealVal n) = k ++ " = " ++ show n
        prettyModelVal (k, ArrayFunc a) = k ++ " = " ++ final ++ "       " -- ++ show (aNull, aLength, a, arrKv, elseVal, length (buildArray 0))
            where (BoolVal aNull) = M.findWithDefault (BoolVal False) (k ++ "?null") modelClean
                  (IntVal aLength) = M.findWithDefault (IntVal (-1)) (k ++ "?length") modelClean
                  [InstElse elseVal] = filter (not . isInst) a
                  arrKv :: [(Integer, ModelVal)]
                  arrKv = filter (\(k, v) -> v /= elseVal) (sort (map (\(InstVal k v) -> (k, v)) (filter isInst a)))
                  isInst :: FuncInst -> Bool
                  isInst (InstVal _ v) = True
                  isInst _             = False
                  isValidArray :: Bool
                  isValidArray = null arrKv || (minIndex >= 0 && maxIndex <= aLength)
                      where minIndex = minimum indices
                            maxIndex = maximum indices
                            indices  = map fst arrKv
                  arrMap :: M.Map Integer ModelVal
                  arrMap = M.fromList arrKv
                  buildArray :: Integer -> [ModelVal]
                  buildArray i = if aLength == 0 then [] else M.findWithDefault elseVal i arrMap : if i + 1 == aLength || i + 1 > 100 then [] else buildArray (i + 1)
                  final :: String
                  final | aNull = "null"
                        | isValidArray = show (buildArray 0) ++ if aLength > 100 then " (TRUNCATED, length: " ++ show aLength ++ ")" else "" --let xs = buildArray 0 in if length xs > 100 then show (take 100 xs) ++ " (TRUNCATED)" else show xs
                        | otherwise = "inconsistent array representation" -- blub2
        -- Remove all occurrences of ArrayRef and ArrayAsConst for easier processing later, also does type casting
        modelCleanFunc :: ModelVal -> ModelVal
        modelCleanFunc (ArrayRef s) = let Just v = M.lookup s modelMap in v
        -- modelCleanFunc (ArrayAsConst n) = ArrayFunc [InstElse n]
        modelCleanFunc x = x
        -- Names of the array variables
        arrays :: [String]
        arrays = nub $ M.keys (M.filter isArray modelClean) ++ mapMaybe arrayName (M.keys modelClean)
        -- Names of the constant variables
        consts :: [String]
        consts = filter (\v -> not (isSuffixOf "?length" v || isSuffixOf "?null" v)) $ M.keys (M.filter isConst modelClean)
        -- Returns Just "a" for "a?length" and "a?null"
        arrayName :: String -> Maybe String
        arrayName s
            | "?length" `isSuffixOf` s = Just $ take (length s - 7) s
            | "?null" `isSuffixOf` s = Just $ take (length s - 5) s
            | otherwise = Nothing
        -- Whether a ModelVal is an array
        isArray :: ModelVal -> Bool
        isArray (ArrayFunc _) = True
        isArray _             = False
        -- Whether a ModelVal is a constant
        isConst :: ModelVal -> Bool
        isConst v = case v of
                         BoolVal _ -> True
                         IntVal _  -> True
                         RealVal _  -> True
                         _         -> False

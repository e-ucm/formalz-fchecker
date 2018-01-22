module LogicIR.Backend.Test (test, testEquality) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import LogicIR.Backend.Rewrite
import LogicIR.Backend.Pretty
import Data.Maybe (fromMaybe)
import Data.List (find, nub)
import System.Random
import Control.Exception
import Data.List
import Data.Map (Map)
import qualified Data.Map as M



type Model = [(LExpr, LExpr)]
type ArrayModel = Map Var [LConst]
empty = []

-- Needed for the ArrayModel key ordering.
instance Ord Var where
  (Var _ name1) `compare` (Var _ name2) = name1 `compare` name2



test :: LExpr -> LExpr -> IO (Bool, Model)
test e1 e2 = return (False, empty)

testEquality :: Int -> LExpr -> LExpr -> IO Bool
testEquality n e1 e2 = do
    testLExpr e1
    testLExpr e2
    return False

---- Takes two LExprs as input. Generates a model for them, a tuple in
---- which the first value indicates whether the two LExprs evaluate to
---- the same boolean value when the generated model is used for substitution,
---- and in which the second value is the generated model itself.
--test :: LExpr -> LExpr -> IO (Bool, Model)
--test e1 e2 = do
--    (result1, model) <- testLExpr e1
--    let primFreeE     = applyModel model $ replaceQuantifiers e2
--    let finalE        = repeatedApplyModel (primFreeE, model)
--    if evalPossible finalE then do
--        let result2 = eval finalE
--        return (result1 == result2, model)
--    else
--        return (False, model)

---- calls testEqualityVerbose but only returns a boolean that indicates
---- whether all n tests were successful.
--testEquality :: Int -> LExpr -> LExpr -> IO Bool
--testEquality n e1 e2 = do
--    (result, models) <- testEqualityVerbose n e1 e2
--    return $ result

---- Calls testRepeatedly and returns whether all booleans in the
---- tuples that testRepeatedly returned were true, and also 
---- returns all models for which test returned false.
--testEqualityVerbose :: Int -> LExpr -> LExpr -> IO (Bool, [Model])
--testEqualityVerbose n e1 e2 = do
--    results <- testRepeatedly n e1 e2
--    -- List of results where test returned false.
--    let testFails = filter (not . fst) results
--    return (null testFails, map snd testFails)

---- Calls "test" n times. The fst return tuple item tells whether the
---- bool that "test" returns was true all n times. The snd is just a 
---- list of n elements that contains all the results from the n test calls.
--testRepeatedly :: Int -> LExpr -> LExpr -> IO [(Bool, Model)]
--testRepeatedly 1 e1 e2 = test e1 e2 >>= \t -> return [t]
--testRepeatedly n e1 e2 = do
--    t <- test e1 e2
--    testRepeatedly (n-1) e1 e2 >>= \l -> return (t : l)

-- Given an LExpr, generates a substitution model for all variables in it,
-- and returns whether the formula evaluates to true or to false when that model
-- is used, and the model itself. If the formula, after substitution, cannot
-- be evaluated - i.e. there's still a variable in the LExpr - an error will be thrown.
testLExpr :: LExpr -> IO (LConst, Model)
testLExpr e = do 
    let (regularVs, (arrayVs, arrayOpVs), quantVs) = findVariables e
    putStrLn $ prettyLExpr e
    primitivesModel      <- generateModel regularVs
    arrayModel           <- generateArrayModel arrayVs
    let primFreeE         = applyModel (primitivesModel, arrayModel) e
    let final             = applyQuantifiers (primitivesModel, arrayModel) e
    putStrLn $ prettyLExpr primFreeE
    --putStrLn $ show primitivesModel
    --putStrLn $ show arrayModel
    --(finalE, arrayModel) <- repeatedApplyAndExpandModel (e, empty)
    --let model             = arrayModel ++ primitivesModel
    --let result            = eval finalE
    --return $ (result, model)
    return $ (CBool True, primitivesModel)

    --where quantFreeE = replaceQuantifiers e

applyQuantifiers :: LExpr -> ArrayModel -> LExpr
applyQuantifiers e arrayModel = foldLExpr algebra
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst c           = LConst c
          uni op e         = evalIfPossible $ LUnop op e
          bin le op re     = evalIfPossible $ LBinop le op re
          iff ge e1 e2     = evalIfPossible $ LIf ge e1 e2
          var v            = substitute (LVar v) models
          quant op v e1 e2 = LQuant op v e1 (substitute e2 models)   -- error "applyModel expects a quantifier-free LExpr."
          arr v e          = substitute (LArray v e) models
          snull v          = substitute (LIsnull v) models
          len v            = substitute (LLen v) models





-- applies substitution of arrays until a "pure" LExpr is the result, or no
-- more substitutions can be applied. If the latter is the case, an error
-- will be thrown. ***NOTE***: this function actually expands the model if
-- necessary. If you don't want this, use repeatedApplyModel
--repeatedApplyAndExpandModel :: (LExpr, Model) -> IO (LExpr, Model)
--repeatedApplyAndExpandModel (e, m) = do
--    if length arrays > 0 then
--        if length substitutableArrays > 0 then do
--            model   <- generateModel substitutableArrays
--            let newE = applyModel model e
--            repeatedApplyAndExpandModel (newE, m ++ model)
--        else
--            error $ "There are unsubstitutable array accesses: " ++ (show arrays)
--    else
--        return (e, m)
--    where arrays                        = findArrays e
--          substitutableArrays           = filter sSubstitutable arrays
--          -- An array access a[i] is substitutable if the indexer
--          -- i doesn't contain variables.
--          sSubstitutable (LArray var e) = evalPossible e

--repeatedApplyModel :: (LExpr, Model) -> LExpr
--repeatedApplyModel (e, m) = if newE /= e then
--                                repeatedApplyModel (newE, m)
--                            else
--                                e
--    where newE = applyModel m e

-- Applies substitution given a Model and an LExpr.
applyModel :: (Model, ArrayModel) -> LExpr -> LExpr
applyModel models = foldLExpr algebra
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst c           = LConst c
          uni op e         = evalIfPossible $ LUnop op e
          bin le op re     = evalIfPossible $ LBinop le op re
          iff ge e1 e2     = evalIfPossible $ LIf ge e1 e2
          var v            = substitute (LVar v) models
          quant op v e1 e2 = LQuant op v e1 (substitute e2 models)   -- error "applyModel expects a quantifier-free LExpr."
          arr v e          = substitute (LArray v e) models
          snull v          = substitute (LIsnull v) models
          len v            = substitute (LLen v) models

-- Substitutes an LExpr if a valid substitute can be found in the given list.
substitute :: LExpr -> (Model, ArrayModel) -> LExpr
substitute e@(LArray v i) (_, arrayModel) = do
    if evalPossible i then 
        let (LConst CInt index) = eval i
        let a = arrayModel M.! v
        let l = length a
        let finalIndex = min 
        LConst $ CInt $ (arrayModel M.! v) !! index
    else e
substitute (LIsnull v)  (_, arrayModel) = LConst (CInt (length (arrayModel M.! v)))
substitute (LLen    v)  (_, arrayModel) = LConst (CBool (0 == length (arrayModel M.! v)))
substitute v            (model, _)      = fromMaybe v (fmap snd $ find (\(old,new) -> v == old) model)

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel exprs = mapM generateModelEntry exprs

generateArrayModel :: [LExpr] -> IO ArrayModel
generateArrayModel es = mapM generateArray es >>= \l -> return (M.fromList l)
    where generateArray e@(LVar v@(Var (TArray (TPrim t)) _)) = do
              len         <- generateIntWithBounds (0,3)
              let b        = bounds t
              expArr      <- mapM (\x -> generatePrimitiveWithBounds t b) [1..len]
              let cnstArr  = map (\(LConst c) -> c) expArr
              return (v, cnstArr)

-- Generates an entry for the substitution Model for a given LExpr.
generateModelEntry :: LExpr -> IO (LExpr, LExpr)
generateModelEntry e@(LVar (Var (TPrim t) _)) = do
    generatePrimitive t >>= \v -> return (e, v)
generateModelEntry e@(LIsnull (Var (TArray _) _)) = do
    generatePrimitive PBool >>= \v -> return (e, v)
generateModelEntry e = do
    error $ "Cannot generate model entry for " ++ show e
    return (e, e)
--generateModelEntry e@(LArray (Var (TArray (TPrim t)) _) _) = do
    --generatePrimitive t >>= \v -> return (e, v)

generatePrimitive :: Primitive -> IO LExpr
generatePrimitive t = generatePrimitiveWithBounds t (bounds t)

-- Generates a random LExpr of a certain Primitive type.
generatePrimitiveWithBounds :: Primitive -> (Int, Int) -> IO LExpr
generatePrimitiveWithBounds t b = do
    v <- getStdRandom (randomR b)
    return $ toLExpr t v

generateIntWithBounds :: (Int, Int) -> IO Int
generateIntWithBounds b = getStdRandom (randomR b)

-- Returns the bounds within which a random value should be generated for
-- some type primitive type.
bounds :: Primitive -> (Int, Int)
bounds PBool  = (0, 1)
bounds PInt32 = (-10, 10)

-- Generates an LExpr given a Primitive type and a value.
toLExpr :: Primitive -> Int -> LExpr
toLExpr PBool  v = LConst $ CBool $ v == 1
toLExpr PInt32 v = LConst $ CInt  $ v

-- The first  list contains all regular var LExpr's.
-- The second item contains a list of all arrays and a list of array operators (isnull / len).
-- The third  list contains all quantifier indexer vars.
type VariableCollection = ([LExpr],([LExpr],[LExpr]),[LExpr])

merge :: VariableCollection -> VariableCollection -> VariableCollection
merge (a1,(b1,c1),d1) (a2,(b2,c2),d2) = (a1++a2,(b1++b2,c1++c2),d1++d2)

findVariables :: LExpr -> VariableCollection
findVariables e = (a \\ c, b, c)
    where algebra :: LExprAlgebra VariableCollection
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst _           = ([],([],[]),[])
          uni _ e          = e
          bin le _ re      = merge le re
          iff ge e1 e2     = merge ge $ merge e1 e2
          var v            = ([LVar v],([],[]),[])
          quant qt v e1 e2 = merge ([],([],[]),[LVar v]) $ merge e1 e2
          arr v e          = merge ([],([LVar v],[]),[]) e
          snull v          = ([],([],[LIsnull v]),[])
          len v            = ([],([],[LLen v]),[])

          (afull,(bfull,cfull),dfull) = foldLExpr algebra e
          (a,b,c) = (nub afull,(nub bfull,nub cfull),nub dfull)


---- Returns a list with all LArrays in the LExpr. 
--findArrays :: LExpr -> [LExpr]
--findArrays expr = nub $ arrs
--    where (arrs,_) = foldLExpr algebra expr
--          algebra :: LExprAlgebra ([LExpr], LExpr)
--          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
--          cnst c          = ([], LConst c)
--          uni op e        = (fst e, LUnop op (snd e))
--          bin le op re    = (fst le ++ fst re, LBinop (snd le) op (snd re))
--          iff ge e1 e2    = (fst ge ++ fst e1 ++ fst e2, LIf (snd ge) (snd e1) (snd e2))
--          var v           = error "findArrays expects an LVar-free LExpr"
--          quant _ _ e1 e2 = error "findArrays expects a quantifier-free LExpr."
--          arr v e         = do
--              let array = LArray v (snd e)
--              (array : (fst e), array)
--          snull v         = ([], LVar v)
--          len v           = ([], LVar v)


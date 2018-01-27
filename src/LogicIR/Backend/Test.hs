module LogicIR.Backend.Test (test, testEquality) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import LogicIR.Backend.Rewrite
import LogicIR.Backend.Pretty
import Data.Maybe (fromMaybe)
import Data.List (find, nub, (\\))
import System.Random
import Control.Exception
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import System.IO.Unsafe


type Model = [(LExpr, LExpr)]
type ArrayModel = Map Var [LConst]
empty = []

-- Needed for the ArrayModel key ordering.
instance Ord Var where
  (Var _ name1) `compare` (Var _ name2) = name1 `compare` name2



test :: LExpr -> LExpr -> IO (Bool, Model, ArrayModel)
test e1 e2 = do
    (res1,primitivesModel,arrayModel) <- testLExpr e1
    (res2,_,_) <- testModel e2 (primitivesModel, arrayModel)
    return (res1 == res2, primitivesModel, arrayModel)

testEquality :: Int -> LExpr -> LExpr -> IO (Bool, (Model, ArrayModel))
testEquality 0 e1 e2 = return (True, (empty, M.empty))
testEquality n e1 e2 = do
    (r,m1,m2) <- test e1 e2
    if not r then do 
        putStrLn $ show r
        putStrLn $ show m1
        putStrLn $ show m2
        return (False, (m1, m2))
    else do
        (rs, model) <- testEquality (n-1) e1 e2
        return ((r && rs), model)

-- Given an LExpr, generates a substitution model for all variables in it,
-- and returns whether the formula evaluates to true or to false when that model
-- is used, and the model itself. If the formula, after substitution, cannot
-- be evaluated - i.e. there's still a variable in the LExpr - an error will be thrown.
testLExpr :: LExpr -> IO (LConst, Model, ArrayModel)
testLExpr e = do 
    let (regularVs, (arrayVs, arrayOpVs), quantVs) = findVariables e
    primitivesModel  <- generateModel regularVs
    arrayModel       <- generateArrayModel arrayVs
    testModel e (primitivesModel, arrayModel)

testModel :: LExpr -> (Model, ArrayModel) -> IO (LConst, Model, ArrayModel)
testModel e (primitivesModel, arrayModel) = do
    let substitutedE = applyModel e (primitivesModel, arrayModel)
    if evalPossible substitutedE then do
        let res = eval substitutedE
        return (res, primitivesModel, arrayModel)
    else
        return (CBool False, primitivesModel, arrayModel)

-- Applies substitution given a Model and an LExpr.
applyModel :: LExpr -> (Model, ArrayModel) -> LExpr
applyModel e models = foldLExpr algebra e
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst             = LConst
          uni op e         = evalIfPossible $ LUnop op e
          bin le op re     = evalIfPossible $ LBinop le op re
          iff ge e1 e2     = evalIfPossible $ LIf ge e1 e2
          var v            = subst (LVar v) models
          quant op v e1 e2 = subst (LQuant op v (subst e1 models) (subst e2 models)) models
          arr v e          = subst (LArray v e) models
          snull v          = subst (LIsnull v) models
          len v            = subst (LLen v) models

tr model v = do
    let res = (M.lookup) v model
    if res == Nothing then do 
        let res = (unsafePerformIO $ putStrLn $ "Is nothing! " ++ (show v) ++ " " ++ (show model))
        []
    else
        fromJust res

-- Substitutes an LExpr if a valid subst can be found in the given models.
-- Otherwise simply returns the input expression as output.
subst :: LExpr -> (Model, ArrayModel) -> LExpr
subst e@(LArray v indexE) (_, arrayModel)
    = if evalPossible indexE then do
        let (CInt index) = eval indexE
        let a            = tr arrayModel v
        let len          = length a
        if index < len && index >= 0 then
            LConst $ a !! index
        else
            e -- Just return the input LExpr, since the index is out of the domain.
    else e
subst   q@(LQuant qop v domain e) models = do
    let op = if qop == QAll then and else or
    let results = evalQuantifier v domain e models
    if any isNothing results
    then q
    else LConst $ CBool $ op $ map fromJust results
subst (LIsnull v)  (_, arrayModel) = LConst (CBool (0 == length (tr arrayModel v)))
subst (LLen    v)  (_, arrayModel) = LConst (CInt (length (tr arrayModel v)))
subst v (model, _)                 = fromMaybe v (fmap snd $ find (\(old,new) -> v == old) model)

-- Returns, given a domain LExpr that concerns a certain Var, all
-- Ints that fall in that domain.
evalQuantifier :: Var -> LExpr -> LExpr -> (Model, ArrayModel) -> [Maybe Bool]
evalQuantifier v domain e (m1,m2) = do
    let newM1s = map (\i -> (LVar v, LConst (CInt i)) : m1) [-20..20]
    concatMap getResult newM1s
    where isTrue expr = (eval expr) == (CBool True)
          getResult newM1 = do
              let newDomain = applyModel domain (newM1,m2)
              if evalPossible newDomain then
                  if isTrue newDomain then do
                      let newE = applyModel e (newM1,m2)
                      if evalPossible newE then
                          if isTrue newE then
                              [Just True]
                          else
                              [Just False]
                      else
                          [Nothing] -- newE cannot be evaluated yet.
                  else
                      []
              else
                  [Nothing] -- newDomain cannot be evaluated yet.

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

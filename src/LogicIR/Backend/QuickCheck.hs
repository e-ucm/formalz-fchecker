module LogicIR.Backend.QuickCheck (check) where

import LogicIR.Expr
import LogicIR.Fold
import LogicIR.Eval
import LogicIR.Backend.Rewrite
import LogicIR.Backend.Pretty
import Test.QuickCheck
import Data.Maybe (fromMaybe)
import Data.List (find, nub)
import System.Random
import Control.Exception

type Model = [(LExpr, LExpr)]
empty :: Model
empty = []

-- Takes two LExprs as input. Generates a model for them, a tuple in
-- which the first value indicates whether the two LExprs evaluate to
-- the same boolean value when the generated model is used for substitution,
-- and in which the second value is the generated model itself.
check :: LExpr -> LExpr -> IO (Bool, Model)
check e1 e2 = do
    (result1, model) <- checkLExpr e1
    let result2       = eval $ applyModel model $ replaceQuantifiers e2
    return (result1 == result2, model)

-- Given an LExpr, generates a substitution model for all variables in it,
-- and returns whether the formula evaluates to true or to false when that model
-- is used, and the model itself. If the formula, after substitution, cannot
-- be evaluated - i.e. there's still a variable in the LExpr - an error will be thrown.
checkLExpr :: LExpr -> IO (LConst, Model)
checkLExpr e = do 
    primitivesModel      <- generateModel $ primitives
    let primFreeE         = applyModel primitivesModel quantFreeE
    (finalE, arrayModel) <- repeatedApplyModel (primFreeE, empty)
    let model             = arrayModel ++ primitivesModel
    let result            = eval finalE
    return $ (result, model)
    where quantFreeE                     = replaceQuantifiers e
          vars                           = findPrimitives quantFreeE
          primitives                     = filter sPrim vars
          sPrim (LVar (Var (TPrim _) _)) = True
          sPrim _                        = False

-- applies substitution of arrays until a "pure" LExpr is the result, or no
-- more substitutions can be applied. If the latter is the case, an error
-- will be thrown.
repeatedApplyModel :: (LExpr, Model) -> IO (LExpr, Model)
repeatedApplyModel (e, m) = do
    if length arrays > 0 then
        if length substitutableArrays > 0 then do
            model   <- generateModel substitutableArrays
            let newE = applyModel model e
            repeatedApplyModel (newE, m ++ model)
        else
            error $ "There are unsubstitutable array accesses: " ++ (show arrays)
    else
        return (e, m)
    where arrays                        = findArrays e
          substitutableArrays           = filter sSubstitutable arrays
          -- An array access a[i] is substitutable if the indexer
          -- i doesn't contain variables.
          sSubstitutable (LArray var e) = evalPossible e

-- Applies substitution given a Model and an LExpr.
applyModel :: Model -> LExpr -> LExpr
applyModel model = foldLExpr algebra
    where algebra :: LExprAlgebra LExpr
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst c          = LConst c
          uni op e        = evalIfPossible $ LUnop op e
          bin le op re    = evalIfPossible $ LBinop le op re
          iff ge e1 e2    = evalIfPossible $ LIf ge e1 e2
          var v           = substitute (LVar v) model
          quant _ _ e1 e2 = error "applyModel expects a quantifier-free LExpr."
          arr v e         = substitute (LArray v e) model
          snull v         = LIsnull v
          len v           = LLen v

-- Substitutes an LExpr if a valid substitute can be found in the given list.
substitute :: LExpr -> Model -> LExpr
substitute v model = fromMaybe v (fmap snd $ find (\(old,new) -> v == old) model)

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel exprs = mapM generateModelEntry exprs
    
-- Generates an entry for the substitution Model for a given LExpr.
generateModelEntry :: LExpr -> IO (LExpr, LExpr)
generateModelEntry e@(LVar (Var (TPrim t) _)) = do
    generateValue t >>= \v -> return (e, v)
generateModelEntry e@(LArray (Var (TArray (TPrim t)) _) _) = do
    generateValue t >>= \v -> return (e, v)

-- Generates a random LExpr of a certain Primitive type.
generateValue :: Primitive -> IO LExpr
generateValue t = do
    v <- getStdRandom (randomR $ (0, 1))
    return $ toLExpr t v

-- Returns the bounds within which a random value should be generated for
-- some type primitive type.
bounds :: Primitive -> (Int, Int)
bounds PBool  = (0, 1)
bounds PInt32 = (-10, 10)

-- Generates an LExpr given a Primitive type and a value.
toLExpr :: Primitive -> Int -> LExpr
toLExpr PBool  v = LConst $ CBool $ v == 1
toLExpr PInt32 v = LConst $ CInt  $ v

-- Returns a list with all Vars in the LExpr.
findPrimitives :: LExpr -> [LExpr]
findPrimitives expr = nub $ foldLExpr algebra expr
    where algebra :: LExprAlgebra [LExpr]
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst _          = []
          uni _ e         = e
          bin le _ re     = le ++ re
          iff ge e1 e2    = ge ++ e1 ++ e2
          var v           = [LVar v]
          quant _ _ e1 e2 = error "findPrimitives expects a quantifier-free LExpr."
          arr v e         = e
          snull v         = [LVar v]
          len v           = [LVar v]

-- Returns a list with all LArrays in the LExpr. 
findArrays :: LExpr -> [LExpr]
findArrays expr = nub $ arrs
    where (arrs,_) = foldLExpr algebra expr
          algebra :: LExprAlgebra ([LExpr], LExpr)
          algebra = (cnst, var, uni, bin, iff, quant, arr, snull, len)
          cnst c          = ([], LConst c)
          uni op e        = (fst e, LUnop op (snd e))
          bin le op re    = (fst le ++ fst re, LBinop (snd le) op (snd re))
          iff ge e1 e2    = (fst ge ++ fst e1 ++ fst e2, LIf (snd ge) (snd e1) (snd e2))
          var v           = error "findArrays expects an LVar-free LExpr"
          quant _ _ e1 e2 = error "findArrays expects a quantifier-free LExpr."
          arr v e         = do
              let array = LArray v (snd e)
              (array : (fst e), array)
          snull v         = ([], LVar v)
          len v           = ([], LVar v)

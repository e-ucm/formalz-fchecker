module LogicIR.Backend.QuickCheck.API (equivalentTo) where

import LogicIR.Expr
import LogicIR.Backend.QuickCheck.ModelGenerator
import LogicIR.Backend.QuickCheck.Test
import LogicIR.Backend.Z3.Model
import LogicIR.Backend.Z3.API (Z3Response (..))
import LogicIR.Pretty (prettyLExpr)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map.Lazy as M

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Z3Response
equivalentTo lexpr lexpr' = do
    (eq, testModel) <- testEquality 1000 lexpr lexpr'
    if eq
    then
        return $ Equivalent
    else
        return $ NotEquivalent $ toZ3Model testModel

toZ3Model :: (Model, ArrayModel) -> Z3Model
toZ3Model (m, arrayM) = do
    let arrayKeys = map (\v -> LVar v) $ M.keys arrayM
    let arrayVals = map (\v -> map LConst v) $ M.elems arrayM
    let arrayKVs = zip arrayKeys (map toModelVals arrayVals)
    let modelKVs = map (\(k,v) -> (k, toModelVal v)) m
    let model = arrayKVs ++ modelKVs
    M.fromList $ map makePretty model
    where makePretty (k,v) = (prettyLExpr k, v)

toModelVal :: LExpr -> ModelVal
toModelVal (LConst (CBool b)) = BoolVal b
toModelVal (LConst (CInt  i)) = IntVal  $ toInteger i
toModelVal (LConst (CReal r)) = RealVal r

toModelVals :: [LExpr] -> ModelVal
toModelVals es@(x:xs) = ManyVal $ map toModelVal es
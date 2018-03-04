module LogicIR.Backend.QuickCheck.API (equivalentTo) where

import Control.Arrow (second)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified LogicIR.Backend.QuickCheck.ModelGenerator as QC
import LogicIR.Backend.QuickCheck.Test
import LogicIR.Expr
import LogicIR.Pretty (prettyLExpr)
import Model

-- | Determine the equality of two method's pre/post conditions.
equivalentTo :: LExpr -> LExpr -> IO Response
equivalentTo lexpr lexpr' = do
    (eq, testModel) <- testEquality 1000 lexpr lexpr'
    if eq
    then return Equivalent
    else return $ NotEquivalent $ toModel testModel

toModel :: (QC.Model, QC.ArrayModel) -> Model
toModel (m, arrayM) = do
    let arrayKeys = map LVar $ M.keys arrayM
    let arrayVals = map (map LConst) $ M.elems arrayM
    let arrayKVs = zip arrayKeys (map toModelVals arrayVals)
    let modelKVs = map (second toModelVal) m
    let model = arrayKVs ++ modelKVs
    M.fromList $ map makePretty model
    where makePretty (k,v) = (prettyLExpr k, v)

toModelVal :: LExpr -> ModelVal
toModelVal (LConst (CBool b)) = BoolVal b
toModelVal (LConst (CInt  i)) = IntVal  $ toInteger i
toModelVal (LConst (CReal r)) = RealVal r

toModelVals :: [LExpr] -> ModelVal
toModelVals es@(x:xs) = ManyVal $ map toModelVal es

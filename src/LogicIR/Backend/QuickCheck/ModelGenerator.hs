{-# LANGUAGE ScopedTypeVariables #-}

module LogicIR.Backend.QuickCheck.ModelGenerator (generateModel, generateArrayModel, generateEvalInfo, expandEvalInfo, findVariables, Model, ArrayModel, EvalInfo) where

import           Data.List     (nub, (\\))
import           Data.Map      (Map)
import qualified Data.Map.Lazy as M
import           System.Random

import LogicIR.Expr
import LogicIR.Fold

type Model = [(LExpr, LExpr)]
type ArrayModel = Map Var [LConst]

-- EvalInfo contains all information needed to evaluate an LExpr.
-- The third tuple element is a list of all Quantifier iterator vars.
type EvalInfo = (Model, ArrayModel, [LExpr])

-- If a model was generated for e1, then it is possible that there are
-- still vars in e2 that do not appear in e1 and therefore not in its
-- model. This is where expandModel comes into play.
expandEvalInfo :: LExpr -> (Model, ArrayModel) -> IO EvalInfo
expandEvalInfo e (m, arrM) = do
    (m', arrM', quantVs') <- generateEvalInfo e
    let mFinal    = m    ++ filter (\(k,_) -> notElem k (map fst m)) m'
    let arrMFinal = M.toList arrM ++ filter (\(k,_) -> notElem k (M.keys arrM)) (M.toList arrM')
    return (mFinal, M.fromList arrMFinal, quantVs')

generateEvalInfo :: LExpr -> IO EvalInfo
generateEvalInfo e = do
    let (regularVs, (arrayVs, arrayOpVs), quantVs) = findVariables e
    let isNullExprs   = filter sIsnullExpr arrayOpVs
    primitivesModel  <- generateModel (regularVs ++ isNullExprs)
    arrayModel       <- generateArrayModel arrayVs
    return (primitivesModel, arrayModel, quantVs)
    where sIsnullExpr (LIsnull _) = True
          sIsnullExpr _           = False

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel = mapM generateModelEntry

generateArrayModel :: [LExpr] -> IO ArrayModel
generateArrayModel es = M.fromList <$> mapM generateArray es
    where
      generateArray :: LExpr -> IO (Var, [LConst])
      generateArray (LVar x@(Var (TArray (TPrim t)) _)) = do
        l         <- rnd (0,4) :: IO Int
        expArr      <- mapM (\_ -> generatePrimitive t) [1..l]
        let cnstArr  = map (\(LConst c) -> c) expArr
        return (x, cnstArr)
      generateArray _ = error "generateArray: non-array argument"

{- Generates an entry for the substitution Model for a given LExpr.
   Note that firstly:  LIsnull can only be applied to arrays since ints/bools
                       cannot be null in Java,
             secondly: LIsnull is not substituted based on the actual value of
                       the array in the
                       ArrayModel. Instead, it's just a random bool that is
                       present in the primitives model. This is because
                       otherwise, the evaluation of an LExpr should be changed
                       drastically. After all, How do you evaluate a[0] or
                       a.length if a == null... -}
generateModelEntry :: LExpr -> IO (LExpr, LExpr)
generateModelEntry e@(LVar (Var (TPrim t) _)) =
    generatePrimitive t >>= \x -> return (e, x)
generateModelEntry e@(LIsnull (Var (TArray _) _)) =
    generatePrimitive PBool >>= \x -> return (e, x)
generateModelEntry e =
    error $ "Cannot generate model entry for " ++ show e

generatePrimitive :: Primitive -> IO LExpr
generatePrimitive PBool = do
    x <- rnd ((0,1) :: (Int,Int))
    return $ LConst $ CBool $ x == 1
generatePrimitive PInt = do
    x <- rnd (-10,10)
    return $ LConst $ CInt x
generatePrimitive PReal = do
    x <- rnd (-500,500)
    let finalV = (x / 50) :: Double
    return $ LConst $ CReal finalV

rnd :: System.Random.Random a => (a, a) -> IO a
rnd = getStdRandom . randomR

-- The first  list contains all regular var LExpr's.
-- The second item contains a list of all arrays and a list of array operators (isnull / len).
-- The third  list contains all quantifier indexer vars.
type VariableCollection = ([LExpr],([LExpr],[LExpr]),[LExpr])

findVariables :: LExpr -> VariableCollection
findVariables e = (k \\ m, l, m)
    where merge :: VariableCollection -> VariableCollection -> VariableCollection
          merge (a1,(b1,c1),d1) (a2,(b2,c2),d2) = (a1++a2,(b1++b2,c1++c2),d1++d2)

          algebra :: LAlgebra VariableCollection
          algebra = LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen
          fcns _         = ([],([],[]),[])
          funi _ x       = x
          fbin le _      = merge le
          fiff ge e1 e2  = merge ge $ merge e1 e2
          fvar x         = ([LVar x],([],[]),[])
          fqnt _ x e1 e2 = merge ([],([],[]),[LVar x]) $ merge e1 e2
          farr x         = merge ([],([LVar x],[]),[])
          fnll x         = ([],([LVar x],[LIsnull x]),[])
          flen x         = ([],([LVar x],[LLen x]),[])

          (afull,(bfull,cfull),dfull) = foldLExpr algebra e
          (k,l,m) = (nub afull,(nub bfull,nub cfull),nub dfull)

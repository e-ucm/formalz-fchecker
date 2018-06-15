{-# LANGUAGE ScopedTypeVariables #-}

module LogicIR.Backend.QuickCheck.ModelGenerator
  ( generateModel
  , generateArrays
  , generateEvalInfo
  , expandEvalInfo
  , findVariables
  , get
  , Model
  , Array (..)
  , ArrayModel (..)
  , EvalInfo) where

import           Data.List     (nub, (\\), find)
import           Data.Map      (Map)
import qualified Data.Map.Lazy as M
import           System.Random

import LogicIR.Expr
import LogicIR.Fold

type Model = [(LExpr, LExpr)]

-- First argument is a list of dimension sizes (so a 2*3*5 array would have
-- [2,3,5]), the second argument are all the actual (1D) arrays (in the case
-- of [2,3,5], this second list would have a length of 30. The first 15
-- elements in this list would belong to [0], the second 15 to [1] etc.).
data Array = Array [Int] [[LExpr]]
  deriving (Show)

data ArrayModel = ArrayModel (Map Var Array) [(Var,LExpr)]
  deriving (Show)

-- EvalInfo contains all information needed to evaluate an LExpr.
-- The third tuple element is a list of all Quantifier iterator vars.
type EvalInfo = (Model, ArrayModel, [LExpr])

-- If a model was generated for e1, then it is possible that there are
-- still vars in e2 that do not appear in e1 and therefore not in its
-- model. This is where expandModel comes into play.
expandEvalInfo :: LExpr -> (Model, ArrayModel) -> IO EvalInfo
expandEvalInfo e (m, arrM) = do
    (m', arrM', quantVs') <- generateEvalInfo e
    return (m', arrM', quantVs')
    -- TODO expand eval info.
    -- let mFinal    = m    ++ filter (\(k,_) -> notElem k (map fst m)) m'
    -- let arrMFinal = M.toList arrM ++ filter (\(k,_) -> notElem k (M.keys arrM)) (M.toList arrM')
    -- return (mFinal, M.fromList arrMFinal, quantVs')

-- (a.length == 1) /\
-- "TEMP_0" == (LArray (Var (TArray (TArray (TPrim PInt))) "a") (LConst (CInt 0))))
-- => (TEMP_0.length == 1)

generateEvalInfo :: LExpr -> IO EvalInfo
generateEvalInfo e = do
    let (regularVs, (arrayVs, arrayOpVs), quantVs) = findVariables e
    putStrLn $ show e
    putStrLn $ "a " ++ show regularVs
    putStrLn $ "b " ++ show arrayVs
    putStrLn $ "c " ++ show arrayOpVs
    putStrLn $ "d " ++ show quantVs
    putStrLn $ "e " ++ show (arrEqs e)
    let isNullExprs   = filter sIsnullExpr arrayOpVs
    let supers        = superArrays arrayVs (arrEqs e)
    primitivesModel  <- generateModel (regularVs ++ isNullExprs)
    arrays           <- generateArrays supers
    let arrayModel = ArrayModel arrays (arrEqs e)
    return (primitivesModel, arrayModel, quantVs)
    where sIsnullExpr (LIsnull _) = True
          sIsnullExpr _           = False

-- Calculates, given a list of LVar's that represent all array variables in the
-- program, which of these arrays cannot be defined in terms of another array.
superArrays :: [LExpr] -> [(Var,LExpr)] -> [LExpr]
superArrays []     _  = []
superArrays (x:xs) ys = case find (\(y,_) -> x == (LVar y)) ys of
  Nothing -> x : superArrays xs ys -- x is super: it can not be defined in terms of another array
  Just _  -> superArrays xs ys

-- Generates a constant value for all vars vars in the given list.
generateModel :: [LExpr] -> IO Model
generateModel = mapM generateModelEntry

generateArrays :: [LExpr] -> IO (Map Var Array)
generateArrays es = M.fromList <$> mapM generateArray es

generateArray :: LExpr -> IO (Var, Array)
generateArray (LVar v@(Var x _)) = do
  let d       = dim x
  -- We only support rectangularly shaped arrays, so we need to generate
  -- (product lengths) actual arrays, each of length (last lengths)
  lengths <- mapM (\_ -> rnd (0,6)) [1..d]
  arrs <- mapM arr (replicate (product lengths) (last lengths))
  return (v, Array lengths arrs)
  where dim (TArray x) = 1 + dim x
        dim _          = 0
        t (TArray x) = t x
        t (TPrim x)  = x
        arr l = mapM (\_ -> generatePrimitive (t x)) [1..l]
generateArray e = error $ "generateArray: non-array argument: " ++ show e

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

-- - The first  list contains all regular var LExpr's.
-- - The second item contains:
--     - a list of all arrays
--     - a list of array operators (isnull / len).
-- - The third  list contains all quantifier indexer vars.
type VariableCollection      = ([LExpr],([LExpr],[LExpr]),[LExpr])

-- Calculates all array equivalences, e.g. a[1] == b would result in [(b,a[1])]
-- TODO: make sure that if we know: a == b[1], b == c[1] that a == c[1][1].
arrEqs :: LExpr -> [(Var,LExpr)]
arrEqs e = snd $ foldLExpr algebra e
  where algebra :: LAlgebra (LExpr,[(Var, LExpr)])
        algebra = LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen
        fcns c    = (LConst c,[])
        funi o e1 = (LUnop o (fst e1),snd e1)
        fbin (a@(LArray v1 i),xs) o@CEqual (b@(LVar v2),ys) =
          (LBinop a o b, (v2, LArray v1 i) : xs ++ ys)
        fbin (a@(LVar v2),xs) o@CEqual (b@(LArray v1 i),ys) =
          (LBinop a o b, (v2, LArray v1 i) : xs ++ ys)
        fbin e1 o e2 = (LBinop (fst e1) o (fst e2),snd e1 ++ snd e2)
        fiff e1 e2 e3 = (LIf (fst e1) (fst e2) (fst e3), snd e1 ++ snd e2 ++ snd e3)
        fvar v = (LVar v,[])
        fqnt o y e1 e2 = (LQuant o y (fst e1) (fst e2),snd e1 ++ snd e2)
        farr v e1 = (LArray v (fst e1), snd e1)
        fnll v = (LIsnull v,[])
        flen v = (LLen v,[])

findVariables :: LExpr -> VariableCollection
findVariables e = ((((k \\ m) \\ (fst l)) \\ (snd l)), l, m)
    where join :: VariableCollection -> VariableCollection -> VariableCollection
          join (a1,(b1,c1),d1) (a2,(b2,c2),d2) = (a1++a2,(b1++b2,c1++c2),d1++d2)

          algebra :: LAlgebra VariableCollection
          algebra = LAlgebra fcns fvar funi fbin fiff fqnt farr fnll flen
          fcns _         = ([],([],[]),[])
          funi _ x       = x
          fbin le _ re   = join le re
          fiff ge e1 e2  = join ge $ join e1 e2
          fvar x         = ([LVar x],([],[]),[])
          fqnt _ x e1 e2 = join ([],([],[]),[LVar x]) $ join e1 e2
          farr x         = join ([],([LVar x],[]),[])
          fnll x         = ([],([LVar x],[LIsnull x]),[])
          flen x         = ([],([LVar x],[LLen x]),[])

          (afull,(bfull,cfull),dfull) = foldLExpr algebra e
          (k,l,m) = (nub afull,(nub bfull,nub cfull),nub dfull)

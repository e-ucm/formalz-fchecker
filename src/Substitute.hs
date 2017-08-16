module Substitute (substVar, desugarAssign) where

import Language.Java.Syntax
import Data.List

import Folds
import HelperFunctions


-- | A type for the inherited attribute
data SubstInh = SubstInh {  lhs         :: Lhs,         -- Left hand side of the assignment
                            rhs         :: Exp,         -- Right hand side
                            env         :: TypeEnv,
                            decls       :: [TypeDecl],   -- Class declarations
                            arrayLookup :: Bool,         -- true iff the current expression is an array that we're trying to access
                            combs       :: Maybe [([Ident], [Ident])] -- The combinations of prefixes we still have to check when substituting a name
                }

substVarExpAlgebra :: ExpAlgebra (SubstInh -> Exp)
substVarExpAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit _ = Lit lit
    fClassLit mt _ = ClassLit mt
    fThis _ = This
    fThisClass name _ = ThisClass name
    fInstanceCreation typeArgs classType args mBody _ = InstanceCreation typeArgs classType args mBody
    fQualInstanceCreation e typeArgs ident args mBody inh = QualInstanceCreation (e inh) typeArgs ident args mBody
    fArrayCreate t exps dim inh = ArrayCreate t (map ($ inh) exps) dim
    fArrayCreateInit t dim arrayInit _ = ArrayCreateInit t dim arrayInit
    fFieldAccess fieldAccess inh                 = case lhs inh of
                                                        FieldLhs fieldAccess' -> case fieldAccess of
                                                                                PrimaryFieldAccess e ident -> error "fieldAccess substitution"
                                                                                SuperFieldAccess ident -> error "fieldAccess substitution"
                                                                                ClassFieldAccess name ident -> error "fieldAccess substitution"
                                                        _ -> FieldAccess fieldAccess
    fMethodInv invocation inh           = case invocation of 
                                            MethodCall name exps -> MethodInv (MethodCall name (map (flip (foldExp substVarExpAlgebra) (inh {arrayLookup = True})) exps))
                                            _                    -> MethodInv invocation
    fArrayAccess (ArrayIndex a i) inh   = let a' = foldExp substVarExpAlgebra a (inh {arrayLookup = True})
                                              i' = map (flip (foldExp substVarExpAlgebra) inh) i in
                                          case lhs inh of
                                            ArrayLhs (ArrayIndex a'' i'') -> Cond (foldr (\(i1, i2) e -> e &* (i1 ==* i2)) (a' ==* a'') (zip i' i'')) (rhs inh) (arrayAccess a' i')
                                            _ -> arrayAccess a' i'
    fExpName (Name name) inh = case combs inh of
                                -- fill in the combs and recurse:
                                Nothing -> case lhs inh of
                                                NameLhs (Name lhsName) -> fExpName (Name name) (inh {combs = Just (getCombs name lhsName)})
                                                _ -> ExpName (Name name)
                                -- done:
                                Just [] -> ExpName (Name name)
                                -- check a combination and recurse when necessary:
                                Just ((nameInit, lhsNameInit) : combs') -> 
                                    case lhs inh of
                                            NameLhs (Name lhsName) -> case lookupType (decls inh) (env inh) (Name lhsNameInit) of
                                                                        PrimType _  | lhsName == name   -> rhs inh
                                                                        RefType _   | isIntroducedVar (Name lhsName) && lhsName == name && not (isObjectCreation (rhs inh)) -> rhs inh
                                                                        RefType _   | isIntroducedVar (Name lhsName) -> ExpName (Name name)
                                                                        RefType t   | lookupType (decls inh) (env inh) (Name nameInit) == RefType t -> case rhs inh of
                                                                                                                                                            ExpName (Name rhsName)      | take (length lhsName) name == lhsName                 -> ExpName (Name (rhsName ++ drop (length lhsName) name))
                                                                                                                                                                                        -- accessing o1.x might affect o2.x if o1 and o2 point to the same object:
                                                                                                                                                                                        | name == nameInit ++ drop (length lhsNameInit) lhsName && length name > length nameInit -> Cond (ExpName (Name nameInit) ==* ExpName (Name lhsNameInit)) (rhs inh) (ExpName (Name name))
                                                                                                                                                                                        | otherwise                                             -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                                                                                                                                            
                                                                                                                                                            -- we substitute instance creation only if we access a field, to not lose pointer information
                                                                                                                                                            -- example: {x = new obj} doesn't affect {x = y} but it does affect {x.a = y.a}
                                                                                                                                                            InstanceCreation _ _ _ _    | length lhsName < length name && take (length lhsName) name == lhsName                     -> getFields (decls inh) (rhs inh) (drop (length lhsName) name)
                                                                                                                                                                                        | length lhsName < length name && take (length lhsName) name == nameInit ++ drop (length nameInit) lhsName    -> Cond (ExpName (Name nameInit) ==* ExpName (Name lhsNameInit)) (getFields (decls inh) (rhs inh) (drop (length lhsName) name)) (ExpName (Name name))
                                                                                                                                                                                        | otherwise                                                                                 -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                                                                                                                                                                        
                                                                                                                                                            -- the same idea for arrays
                                                                                                                                                            ArrayCreate _ _ _           | not $ arrayLookup inh   -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                                                                                                                                            ArrayCreateInit _ _ _       | not $ arrayLookup inh   -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                                                                                                                                                                        
                                                                                                                                                            _                           | take (length lhsName) name == lhsName                 -> getFields (decls inh) (rhs inh) (drop (length lhsName) name)
                                                                                                                                                                                        | name == nameInit ++ drop (length lhsNameInit) lhsName  -> Cond (ExpName (Name nameInit) ==* ExpName (Name lhsNameInit)) (rhs inh) (ExpName (Name name))
                                                                                                                                                                                        -- the assignment doesn't affect this expression:
                                                                                                                                                                                        | otherwise -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                                                        _ -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
                                            _ -> foldExp substVarExpAlgebra (ExpName (Name name)) (inh {combs = Just combs'})
    fPostIncrement e inh = PostIncrement (e inh)
    fPostDecrement e inh = PostDecrement  (e inh)
    fPreIncrement e inh = PreIncrement (e inh)
    fPreDecrement e inh = PreDecrement  (e inh)
    fPrePlus e inh = PrePlus (e inh)
    fPreMinus e inh = PreMinus (e inh)
    fPreBitCompl e inh = PreBitCompl (e inh)
    fPreNot e inh = PreNot (e inh)
    fCast t e inh = Cast t (e inh)
    fBinOp e1 op e2 inh = BinOp (e1 inh) op (e2 inh)
    fInstanceOf e refType inh = InstanceOf (e inh) refType
    fCond g e1 e2 inh = Cond (g inh {arrayLookup = False}) (e1 inh) (e2 inh)
    fAssign lhs assOp e inh = Assign lhs assOp (e inh)
    fLambda lParams lExp _ = Lambda lParams lExp
    fMethodRef className methodName _ = MethodRef className methodName
    
    -- Checks if an expressions creates a new object or array
    isObjectCreation :: Exp -> Bool
    isObjectCreation (InstanceCreation _ _ _ _) = True
    isObjectCreation (ArrayCreate _ _ _)        = True
    isObjectCreation _                          = False
    
    -- Recursively accesses a field of an expression
    getFields :: [TypeDecl] -> Exp -> [Ident] -> Exp
    getFields decls e                           []       = e
    getFields decls (InstanceCreation _ t _ _)  (f:fs)   = getFields decls (getInitValue (getFieldType decls (RefType (ClassRefType t)) (Name [f]))) fs
    getFields decls e                           (f : fs) = getFields decls (FieldAccess (PrimaryFieldAccess e f)) fs
            
    -- for arguments xs and ys, gets all combinations of non-empty prefixes xs' of xs and ys' of ys
    getCombs :: [a] -> [a] -> [([a], [a])]
    getCombs xs ys = [(x, y) | x <- xs', y <- ys'] where
        xs' = drop 1 (inits xs)
        ys' = drop 1 (inits ys)
 
-- | Desugars to a basic assignment, returning the new righ hand side. For example: desugaring x += 3 returns the new rhs x + 3
desugarAssign :: Lhs -> AssignOp -> Exp -> Exp
desugarAssign lhs op e = case op of
                            EqualA -> e
                            MultA -> BinOp e Mult (lhsToExp lhs)
                            DivA -> BinOp e Div (lhsToExp lhs)
                            RemA -> BinOp e Rem (lhsToExp lhs)
                            AddA -> BinOp e Add (lhsToExp lhs)
                            SubA -> BinOp e Sub (lhsToExp lhs)
                            LShiftA -> BinOp e LShift (lhsToExp lhs)
                            RShiftA -> BinOp e RShift (lhsToExp lhs)
                            RRShiftA -> BinOp e RRShift (lhsToExp lhs)
                            AndA -> BinOp e And (lhsToExp lhs)
                            XorA -> BinOp e Xor (lhsToExp lhs)
                            OrA -> BinOp e Or (lhsToExp lhs)
    where 
        lhsToExp (NameLhs name)         = ExpName name
        lhsToExp (FieldLhs fieldAccess) = FieldAccess fieldAccess
        lhsToExp (ArrayLhs arrayIndex)  = ArrayAccess arrayIndex
        
-- | Substitutes all occurences of a specific free variable by an expression
substVar :: TypeEnv -> [TypeDecl] -> Lhs -> Exp -> Exp -> Exp
substVar env decls lhs rhs e = foldExp substVarExpAlgebra e (SubstInh lhs rhs env decls False Nothing)
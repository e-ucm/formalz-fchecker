module Substitute where

import Language.Java.Syntax
import Data.List

import Folds
import HelperFunctions
 

substVarExpAlgebra :: ExpAlgebra ((Lhs, Exp, TypeEnv, [TypeDecl]) -> Exp)
substVarExpAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit _ = Lit lit
    fClassLit mt _ = ClassLit mt
    fThis _ = This
    fThisClass name _ = ThisClass name
    fInstanceCreation typeArgs classType args mBody _ = InstanceCreation typeArgs classType args mBody
    fQualInstanceCreation e typeArgs ident args mBody inh = QualInstanceCreation (e inh) typeArgs ident args mBody
    fArrayCreate t exps dim inh = ArrayCreate t (map ($ inh) exps) dim
    fArrayCreateInit t dim arrayInit _ = ArrayCreateInit t dim arrayInit
    fFieldAccess fieldAccess (lhs, rhs, env, decls) = case lhs of
                                                        FieldLhs fieldAccess' -> case fieldAccess of
                                                                                PrimaryFieldAccess e ident -> error "todo: fieldAccess substitution"
                                                                                SuperFieldAccess ident -> error "todo: fieldAccess substitution"
                                                                                ClassFieldAccess name ident -> error "todo: fieldAccess substitution"
                                                        _ -> FieldAccess fieldAccess
    fMethodInv invocation _ = MethodInv invocation
    fArrayAccess (ArrayIndex a i) (lhs, rhs, env, decls) =  let a' = substVar env decls lhs rhs a
                                                                i' = map (substVar env decls lhs rhs) i in
                                                            case lhs of
                                                                ArrayLhs (ArrayIndex a'' i'') -> Cond (foldr (\(i1, i2) e -> e &* (i1 ==* i2)) (a' ==* a'') (zip i' i'')) rhs (ArrayAccess (ArrayIndex a' i'))
                                                                _ -> ArrayAccess (ArrayIndex a' i')
    fExpName (Name name) (lhs, rhs, env, decls)    = case lhs of
                                                        NameLhs (Name name') -> case lookupType decls env (Name name')  of
                                                                                    PrimType _  | name' == name -> rhs 
                                                                                                | otherwise     -> ExpName (Name name)
                                                                                    RefType t  -> let getFields e []       = e
                                                                                                      getFields e (f : fs) = getFields (FieldAccess (PrimaryFieldAccess e f)) fs
                                                                                                  in if [head name] == name' then getFields rhs (tail name) else Cond (ExpName (Name [head name]) ==* ExpName (Name name')) (getFields rhs (tail name)) (ExpName (Name name))
                                                
                                                
                                                
                                                                 --   | name' == name         -> rhs 
                                                                 --   | isPrefixOf name' name -> let getFields e [f]      = FieldAccess (PrimaryFieldAccess e f)
                                                                 --                                  getFields e (f : fs) = getFields (FieldAccess (PrimaryFieldAccess e f)) fs
                                                                 --                              in  getFields rhs (drop (length name') name)
                                                                 --   | otherwise             -> Cond (ExpName (Name [head name]) ==* ExpName (Name [head name'])) rhs (ExpName (Name name))
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
    fCond g e1 e2 inh = Cond (g inh) (e1 inh) (e2 inh)
    fAssign lhs assOp e inh = Assign lhs assOp (e inh)
    fLambda lParams lExp _ = Lambda lParams lExp
    fMethodRef className methodName _ = MethodRef className methodName
 
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
        lhsToExp (NameLhs name) = ExpName name
        lhsToExp (FieldLhs fieldAccess) = undefined
        lhsToExp (ArrayLhs arrayIndex) = undefined
        
-- | Substitutes all occurences of a specific free variable by an expression
substVar :: TypeEnv -> [TypeDecl] -> Lhs -> Exp -> Exp -> Exp
substVar env decls lhs rhs e = foldExp substVarExpAlgebra e (lhs, rhs, env, decls)
-- Helper functions for the Java data structure
module HelperFunctions where

import Language.Java.Syntax
import Language.Java.Pretty
import Data.Maybe

type TypeEnv = [(Name, Type)]

-- | Retrieves the type from the environment
lookupType :: [TypeDecl] -> TypeEnv -> Name -> Type
lookupType decls env (Name idents) = case lookup (Name [head idents]) env of
                                        Just t  -> getFieldType decls t (Name (tail idents))
                                        Nothing -> error ("can't find type of " ++ prettyPrint (Name idents) ++ "\r\n TypeEnv: " ++ show env)
                                        
-- | Gets the type of a field of an object of given type
getFieldType :: [TypeDecl] -> Type -> Name -> Type
getFieldType _ t (Name []) = t
getFieldType decls (RefType (ClassRefType t)) (Name (f:fs)) = getFieldType decls (getFieldTypeFromClassDecl (getDecl t decls) f) (Name fs)
    where
        getFieldTypeFromClassDecl :: ClassDecl -> Ident -> Type
        getFieldTypeFromClassDecl (ClassDecl _ _ _ _ _ (ClassBody decls)) id = getFieldTypeFromMemberDecls decls id
        
        getFieldTypeFromMemberDecls :: [Decl] -> Ident -> Type
        getFieldTypeFromMemberDecls [] _ = error "getFieldTypeFromMemberDecls"
        getFieldTypeFromMemberDecls (MemberDecl (FieldDecl mods t (VarDecl varId _ : vars)) : decls) id = if getId varId == id then t else getFieldTypeFromMemberDecls (MemberDecl (FieldDecl mods t vars) : decls) id
        getFieldTypeFromMemberDecls (_ : decls) id = getFieldTypeFromMemberDecls decls id
        
         -- Gets the class declaration that matches a given type
        getDecl :: ClassType -> [TypeDecl] -> ClassDecl
        getDecl t@(ClassType [(ident, typeArgs)]) (x:xs)    = case x of
                                                                ClassTypeDecl decl@(ClassDecl _ ident' _ _ _ _) -> if ident == ident' then decl else getDecl t xs
                                                                _ -> getDecl t xs
        getDecl _ _ = error "nested class"
        
getId :: VarDeclId -> Ident
getId (VarId id) = id
getId (VarDeclArray id) = getId id
        
true :: Exp
true = Lit (Boolean True)

false :: Exp
false = Lit (Boolean False)
    
    
-- Logical operators for expressions:
(&*) :: Exp -> Exp -> Exp
e1 &* e2 = BinOp e1 And e2

(|*) :: Exp -> Exp -> Exp
e1 |* e2 = BinOp e1 Or e2

neg :: Exp -> Exp
neg = PreNot

imp :: Exp -> Exp -> Exp
e1 `imp` e2 =  (e1 &* e2) |* neg e1

(==*) :: Exp -> Exp -> Exp
e1 ==* e2 = BinOp e1 Equal e2

(/=*) :: Exp -> Exp -> Exp
e1 /=* e2 = neg (e1 ==* e2)

-- | Gets the initial value for a given type
getInitValue :: Type -> Exp
getInitValue (PrimType t) = case t of
                                BooleanT -> Lit (Boolean False)
                                ByteT -> Lit (Word 0)
                                ShortT -> Lit (Int 0)
                                IntT -> Lit (Int 0)
                                LongT -> Lit (Int 0)
                                CharT -> Lit (Char '\NUL')
                                FloatT -> Lit (Float 0)
                                DoubleT -> Lit (Double 0)
getInitValue (RefType t)  = Lit Null
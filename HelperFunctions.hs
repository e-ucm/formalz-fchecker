-- Helper functions for the Java data structure
module HelperFunctions where

import Language.Java.Syntax
import Language.Java.Pretty
import Data.Maybe
import System.IO.Unsafe
import Data.IORef



type TypeEnv    = [(Name, Type)]
type CallCount  = [(Ident, Int)]

-- | Retrieves the type from the environment
lookupType :: [TypeDecl] -> TypeEnv -> Name -> Type
lookupType decls env (Name ((Ident s@('$':_)) : idents)) = getFieldType decls (getReturnVarType decls s) (Name idents) -- Names starting with a '$' symbol are generated and represent the return variable of a function
lookupType decls env (Name ((Ident s@('#':_)) : idents)) = PrimType undefined -- Names starting with a '#' symbol are generated and represent a variable introduced by handling operators
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
        getDecl t _ = error ("fieldType: " ++ show t)
        
-- | Gets the type of the class in which the method is defined
getMethodClassType :: [TypeDecl] -> Ident -> Type
getMethodClassType decls id = head $ concatMap (flip getMethodTypeFromClassDecl id) decls
    where
        getMethodTypeFromClassDecl :: TypeDecl -> Ident -> [Type]
        getMethodTypeFromClassDecl (ClassTypeDecl (ClassDecl _ className _ _ _ (ClassBody decls))) id = getMethodTypeFromMemberDecls (RefType (ClassRefType (ClassType [(className , [])]))) decls id
        getMethodTypeFromClassDecl _ _ = []
        
        getMethodTypeFromMemberDecls :: Type -> [Decl] -> Ident -> [Type]
        getMethodTypeFromMemberDecls t [] _ = []
        getMethodTypeFromMemberDecls t (MemberDecl (MethodDecl _ _ _ id' _ _ _) : decls) id = if id' == id then [t] else getMethodTypeFromMemberDecls t decls id
        getMethodTypeFromMemberDecls t (_ : decls) id = getMethodTypeFromMemberDecls t decls id
        
-- | Adds the special variables *obj, returnValue and returnValueVar to a type environment, given the id of the method we're looking at
extendEnv :: TypeEnv -> [TypeDecl] -> Ident -> TypeEnv
extendEnv env decls methodId = case getMethodType decls methodId of
                                Nothing -> (Name [Ident "*obj"], getMethodClassType decls methodId) : env
                                Just t  -> (Name [Ident "returnValue"], t) : (Name [Ident "returnValueVar"], t) : (Name [Ident "*obj"], getMethodClassType decls methodId) : env

-- | Creates a string that that represents the return var name of a method call. This name is extended by a number to indicate the depth of the recursive calls
makeReturnVarName :: Ident -> String
makeReturnVarName (Ident s) = "$" ++ s ++ "$"
        
-- | Get's the type of a generated variable
getReturnVarType :: [TypeDecl] -> String -> Type
getReturnVarType decls s = case getMethodType decls (Ident (takeWhile (/= '$') (tail s))) of
                            Nothing -> PrimType undefined -- Kind of a hack. In case of library functions, it doesn't matter what type we return.
                            Just t  -> t
        
-- Increments the call count for a given method
incrCallCount :: CallCount -> Ident -> CallCount
incrCallCount [] id             = [(id, 1)]
incrCallCount ((id', c):xs) id  = if id == id' then (id', c + 1) : xs else (id', c) : incrCallCount xs id


-- Looks up the call count for a given method
getCallCount :: CallCount -> Ident -> Int
getCallCount [] id             = 0
getCallCount ((id', c):xs) id  = if id == id' then c else getCallCount xs id
        
getId :: VarDeclId -> Ident
getId (VarId id) = id
getId (VarDeclArray id) = getId id

fromName :: Name -> [Ident]
fromName (Name name) = name

-- Gets the ident of the method from a name
getMethodId :: Name -> Ident
getMethodId = last . fromName

-- Gets the statement(-block) defining a method
getMethod :: [TypeDecl] -> Ident -> Maybe Stmt
getMethod classTypeDecls methodId = fmap (\(b, _, _) -> b) (getMethod' classTypeDecls methodId)

-- Gets the return type of a method
getMethodType :: [TypeDecl] -> Ident -> Maybe Type
getMethodType classTypeDecls methodId = getMethod' classTypeDecls methodId >>= (\(_, t, _) -> t)

-- Gets the parameter declarations of a method
getMethodParams :: [TypeDecl] -> Ident -> Maybe [FormalParam]
getMethodParams classTypeDecls methodId = fmap (\(_, _, params) -> params) (getMethod' classTypeDecls methodId)

-- Finds a method definition. This function assumes all methods are named differently
getMethod' :: [TypeDecl] -> Ident -> Maybe (Stmt, Maybe Type, [FormalParam])
getMethod' classTypeDecls methodId = case (concatMap searchClass classTypeDecls) of
                                        r:_     -> Just r
                                        []      -> Nothing -- Library function call
  where
    searchClass (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = searchDecls decls
    searchClass _ = []
    searchDecls (MemberDecl (MethodDecl _ _ t id params _ (MethodBody (Just b))):_) | methodId == id = [(StmtBlock b, t, params)]
    searchDecls (MemberDecl (ConstructorDecl _ _ id params _ (ConstructorBody _ b)):_) | methodId == toConstrId id = [(StmtBlock (Block b), Just (RefType (ClassRefType (ClassType [(id, [])]))), params)]
    searchDecls (_:decls) = searchDecls decls
    searchDecls [] = []
    -- Adds a '#' to indicate the id refers to a constructor method
    toConstrId (Ident s) = Ident ('#' : s)

-- Gets the statement(-block) defining the main method
getMainMethod :: [TypeDecl] -> Stmt
getMainMethod classTypeDecls = fromJust' "getMainMethod" $ getMethod classTypeDecls (Ident "main")

-- Gets a list of all method Idents (except constructor methods)
getMethodIds :: [TypeDecl] -> [Ident]
getMethodIds classTypeDecls = concatMap searchClass classTypeDecls where
    searchClass (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = searchDecls decls
    searchClass _ = []
    searchDecls (MemberDecl (MethodDecl _ _ _ id _ _ _):decls) = id : searchDecls decls
    searchDecls (_:decls) = searchDecls decls
    searchDecls [] = []

-- Gets the class declarations
getDecls :: CompilationUnit -> [TypeDecl]
getDecls (CompilationUnit _ _ classTypeDecls) = classTypeDecls
    
-- Checks if the var is introduced. Introduced variable names start with '$' voor return variables of methods and '#' for other variables
isIntroducedVar :: Name -> Bool
isIntroducedVar (Name (Ident ('#':_): _)) = True
isIntroducedVar (Name (Ident ('$':_): _)) = True
isIntroducedVar _ = False

-- Gets the variable that represents the return value of the method
getReturnVar :: MethodInvocation -> Ident
getReturnVar invocation = Ident (makeReturnVarName (invocationToId invocation) ++ show (getIncrPointer varPointer))

-- Gets the method Id from an invocation
invocationToId :: MethodInvocation -> Ident
invocationToId (MethodCall name _) = getMethodId name
invocationToId (PrimaryMethodCall _ _ id _) = id
invocationToId _ = undefined

-- Gets the type of the elements of an array. Recursion is needed in the case of multiple dimensional arrays
arrayContentType :: Type -> Type
arrayContentType (RefType (ArrayType t)) = arrayContentType t
arrayContentType t = t

-- Gets a new unique variable
getVar :: Ident
getVar = Ident ('#' : show (getIncrPointer varPointer))
    
-- Gets multiple new unique variables
getVars :: Int -> [Ident]
getVars 0 = []
getVars n = Ident ('#' : show (getIncrPointer varPointer)) : getVars (n-1)

-- The number of variables introduced
varPointer :: IORef Integer
varPointer = unsafePerformIO $ newIORef 0

--- | Gets the current var pointer and increases the pointer by 1
getIncrPointer :: IORef Integer -> Integer
getIncrPointer ref = unsafePerformIO $
    do
        p <- readIORef ref
        writeIORef ref (p + 1)
        return p
    
-- Used for debugging
fromJust' :: String -> Maybe a -> a
fromJust' s ma = case ma of
                    Nothing -> error s
                    Just x  -> x
        
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
e1 `imp` e2 =  neg e1 |* e2

(==*) :: Exp -> Exp -> Exp
e1 ==* e2 = BinOp e1 Equal e2

(/=*) :: Exp -> Exp -> Exp
e1 /=* e2 = neg (e1 ==* e2)

-- Gets the value from an array
arrayAccess :: Exp -> [Exp] -> Exp
arrayAccess a i = case a of
                    ArrayCreate t exps dim          -> getInitValue t
                    ArrayCreateInit t dim arrayInit -> getInitValue t
                    _                               -> ArrayAccess (ArrayIndex a i)

-- Accesses fields of fields
fieldAccess :: Exp -> Name -> FieldAccess
fieldAccess e (Name [id])       = PrimaryFieldAccess e id
fieldAccess e (Name (id:ids))   = fieldAccess (FieldAccess (PrimaryFieldAccess e id)) (Name ids)
fieldAccess _ _ = error "FieldAccess without field name"
                    
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
-- Copyright (c) 2017 Utrecht University
-- Author: Koen Wermer

-- Helper functions for the Java data structure
module Javawlp.Engine.HelperFunctions where

import Javawlp.Engine.Folds

import Data.IORef
import Data.List
import Data.Maybe
import Debug.Trace
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe


type TypeEnv    = [(Name, Type)]
type CallCount  = [(Ident, Int)]

prettyprintTypeEnv :: TypeEnv -> String
prettyprintTypeEnv env = intercalate "\n" $ map show env

-- | Retrieves the type from the environment
lookupType :: [TypeDecl] -> TypeEnv -> Name -> Type
lookupType decls env (Name (Ident s@('$':_) : idents)) = getFieldType decls (getReturnVarType decls s) (Name idents) -- Names starting with a '$' symbol are generated and represent the return variable of a function
lookupType decls env (Name (Ident s@('#':_) : idents)) = PrimType undefined -- Names starting with a '#' symbol are generated and represent a variable introduced by handling operators
lookupType decls env (Name idents) = case lookup (Name [head idents]) env of
                                        Just t  -> getFieldType decls t (Name (tail idents))
                                        Nothing -> PrimType IntT -- For now we assume library variables to be ints      error ("can't find type of " ++ prettyPrint (Name idents) ++ "\r\n TypeEnv: " ++ show env)

-- | Gets the type of a field of an object of given type
getFieldType :: [TypeDecl] -> Type -> Name -> Type
getFieldType _ t (Name []) = t
getFieldType _ _ (Name [Ident "length"]) = PrimType IntT
getFieldType decls (RefType (ClassRefType t)) (Name (f:fs)) = getFieldType decls (getFieldTypeFromClassDecl (getDecl t decls) f) (Name fs)
    where
        getFieldTypeFromClassDecl :: ClassDecl -> Ident -> Type
        getFieldTypeFromClassDecl (ClassDecl _ _ _ _ _ (ClassBody decls)) = getFieldTypeFromMemberDecls decls

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
getMethodClassType decls id = head $ concatMap (`getMethodTypeFromClassDecl` id) decls
    where
        getMethodTypeFromClassDecl :: TypeDecl -> Ident -> [Type]
        getMethodTypeFromClassDecl (ClassTypeDecl (ClassDecl _ className _ _ _ (ClassBody decls))) id = getMethodTypeFromMemberDecls (RefType (ClassRefType (ClassType [(className , [])]))) decls id
        getMethodTypeFromClassDecl _ _ = []

        getMethodTypeFromMemberDecls :: Type -> [Decl] -> Ident -> [Type]
        getMethodTypeFromMemberDecls t [] _ = []
        getMethodTypeFromMemberDecls t (MemberDecl (MethodDecl _ _ _ id' _ _ _ _) : decls) id = if id' == id then [t] else getMethodTypeFromMemberDecls t decls id
        getMethodTypeFromMemberDecls t (_ : decls) id = getMethodTypeFromMemberDecls t decls id

-- | Adds the special variables *obj, returnValue and returnValueVar to a type environment, given the id of the method we're looking at
extendEnv :: TypeEnv -> [TypeDecl] -> Ident -> TypeEnv
extendEnv env decls methodId = case getMethodType decls methodId of
                                Nothing -> (Name [Ident "*obj"], getMethodClassType decls methodId) : env
                                Just (RefType _)  -> (Name [Ident "returnValue"], returnValueType) : (Name [Ident "returnValueVar"], returnValueType) : (Name [Ident "*obj"], getMethodClassType decls methodId) : env
                                Just t  -> (Name [Ident "returnValue"], t) : (Name [Ident "returnValueVar"], t) : (Name [Ident "*obj"], getMethodClassType decls methodId) : env


-- | We introduce a special type for the return value,
returnValueType :: Type
returnValueType = RefType (ClassRefType (ClassType [(Ident "ReturnValueType", [])]))

-- | Get's the type of a generated variable
getReturnVarType :: [TypeDecl] -> String -> Type
getReturnVarType decls s =
  -- Kind of a hack. In case of library functions, it doesn't matter what type we return.
  fromMaybe (PrimType undefined) $ getMethodType decls (Ident (takeWhile (/= '$') (tail s)))

-- Increments the call count for a given method
incrCallCount :: CallCount -> Ident -> CallCount
incrCallCount [] id             = [(id, 1)]
incrCallCount ((id', c):xs) id  = if id == id' then (id', c + 1) : xs else (id', c) : incrCallCount xs id


-- Looks up the call count for a given method
getCallCount :: CallCount -> Ident -> Int
getCallCount [] id            = 0
getCallCount ((id', c):xs) id = if id == id' then c else getCallCount xs id

getId :: VarDeclId -> Ident
getId (VarId id)        = id
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
getMethod' classTypeDecls methodId = case concatMap searchClass classTypeDecls of
                                        r:_ -> Just r
                                        []  -> Nothing -- Library function call
  where
    searchClass (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = searchDecls decls
    searchClass _ = []
    searchDecls (MemberDecl (MethodDecl _ _ t id params _ _ (MethodBody (Just b))):_) | methodId == id = [(StmtBlock b, t, params)]
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
getMethodIds = concatMap searchClass where
    searchClass (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody decls))) = searchDecls decls
    searchClass _ = []
    searchDecls (MemberDecl (MethodDecl _ _ _ id _ _ _ _):decls) = id : searchDecls decls
    searchDecls (_:decls) = searchDecls decls
    searchDecls [] = []

-- Gets the class declarations
getDecls :: CompilationUnit -> [TypeDecl]
getDecls (CompilationUnit _ _ classTypeDecls) = classTypeDecls

-- Checks if the var is introduced. Introduced variable names start with '$' voor return variables of methods and '#' for other variables
isIntroducedVar :: Name -> Bool
isIntroducedVar (Name (Ident ('#':_): _)) = True
isIntroducedVar (Name (Ident ('$':_): _)) = True
isIntroducedVar _                         = False

-- Gets the variable that represents the return value of an invocation of a method
getReturnVar :: MethodInvocation -> Ident
getReturnVar invocation = Ident (name ++  "___retval" ++ show (getIncrVarMethodInvokesCount methodid))
   where
   methodid@(Ident name) = invocationToId invocation

-- Gets the method Id from an invocation
invocationToId :: MethodInvocation -> Ident
invocationToId (MethodCall name _)          = getMethodId name
invocationToId (PrimaryMethodCall _ _ id _) = id
invocationToId _                            = undefined

-- Gets the type of the elements of an array. Recursion is needed in the case of multiple dimensional arrays
arrayContentType :: Type -> Type
arrayContentType (RefType (ArrayType t)) = arrayContentType t
arrayContentType t                       = t

-- Gets a new unique variable
getVar :: Ident
getVar = Ident ('#' : show (getIncrVarPointer ()))

-- Gets multiple new unique variables
getVars :: Int -> [Ident]
getVars 0 = []
getVars n = Ident ('#' : show (getIncrVarPointer ())) : getVars (n-1)

-- The number of new variables introduced ; also used to assign new variable names
varPointer :: IORef Int
varPointer = unsafePerformIO $ newIORef 0

resetVarPointer = do { writeIORef varPointer 0 ; readIORef varPointer }

-- | Gets the current var-pointer and increases the pointer by 1.
-- Don't drop the dummy () argument; this is to force a re-evaluation of the expression. Otherwise we will
-- get the same integer every time.
getIncrVarPointer :: () -> Int
getIncrVarPointer () = unsafePerformIO $ do
        p <- readIORef varPointer
        writeIORef varPointer (p + 1)
        return p

-- | To keep track of the number of times each method is invoked; also used to assign unique return-value
-- name to each method invocation.
varMethodInvokesCount :: IORef CallCount
varMethodInvokesCount =  unsafePerformIO $ newIORef []

resetVarMethodInvokesCount = do { writeIORef varMethodInvokesCount [] ; readIORef varMethodInvokesCount }

getIncrVarMethodInvokesCount :: Ident -> Int
getIncrVarMethodInvokesCount methodid = unsafePerformIO $ do
        callcount <- readIORef varMethodInvokesCount
        let callcount' = incrCallCount callcount methodid
        let k = getCallCount callcount' methodid
        writeIORef varMethodInvokesCount callcount'
        return k

-- Used for debugging
fromJust' :: String -> Maybe a -> a
fromJust' s = fromMaybe (error s)

true :: Exp
true = Lit (Boolean True)

false :: Exp
false = Lit (Boolean False)


-- Logical operators for expressions:
(&*) :: Exp -> Exp -> Exp
e1 &* e2 = BinOp e1 CAnd e2

(|*) :: Exp -> Exp -> Exp
e1 |* e2 = BinOp e1 COr e2

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
                                ByteT    -> Lit (Word 0)
                                ShortT   -> Lit (Int 0)
                                IntT     -> Lit (Int 0)
                                LongT    -> Lit (Int 0)
                                CharT    -> Lit (Char '\NUL')
                                FloatT   -> Lit (Float 0)
                                DoubleT  -> Lit (Double 0)
getInitValue (RefType t)  = Lit Null

-- counting expressing complexity, which is the number of logical operators in the expression
exprComplexity :: Exp -> Int
exprComplexity e = case e of
    -- recursions
    PreNot e        -> exprComplexity e + 1
    BinOp e1 op e2  ->
        let
        k1 = exprComplexity e1
        k2 = exprComplexity e2
        in
        case op of
         And  -> k1 + k2 + 1
         Or   -> k1 + k2 + 1
         Xor  -> k1 + k2 + 1
         CAnd -> k1 + k2 + 1
         COr  -> k1 + k2 + 1
         _    -> 1
    Cond g e1 e2   -> exprComplexity e1 + exprComplexity e2 + exprComplexity g + 1
    -- other cases are 0
    _              ->  0



findInfix [] s = Just 0
findInfix u s  = find (>=0) [ if u `isPrefixOf` z then i else -1| (i,z) <- tails_ ]
    where
    tails_ = [ (getStartPosition v, getString v) | v <- tails (zip [0..] s) ]
    getString = map snd
    getStartPosition [] = -1
    getStartPosition  v = head (map fst v)

getCallCount__ (ExpName (Name [Ident s])) = case k of
    Just pos -> read $ drop (pos + length key) s
    _        -> -1
    where
    key = "___retval"
    k = findInfix key s

getCallCount__ _ = -1

adjustCallCount__ (ExpName (Name [Ident s])) newCount = ExpName (Name [Ident (s0 ++ key ++ show newCount)])
    where
    key = "___retval"
    k = fromJust $ findInfix key s
    s0 = take k s

getMethodId__ (ExpName (Name [Ident s])) = Ident (take k s)
    where
    key = "___retval"
    k = fromJust $ findInfix key s

normalizeInvocationNumbers :: Exp -> Exp
normalizeInvocationNumbers e = normalize e
   where
   calls_ :: [(Ident,[Int])]
   calls_ = countCallVars [] e

   callscount :: CallCount
   callscount = [ (i,length (nub instances)) | (i,instances) <- calls_ ]


   normalizeName e@(ExpName (Name [ident])) = if k>=0 then adjustCallCount__ e (k `mod` m) else e
        where
        k = getCallCount__ e
        m = getCallCount callscount (getMethodId__ e)

   normalizeName e = e


   normalize e = case e of
       -- base case
       ename@(ExpName _) -> normalizeName ename

       -- recursions
       PrePlus e -> PrePlus (normalize e)
       PreMinus e -> PreMinus (normalize e)
       PreBitCompl e -> PreBitCompl (normalize e)
       PreNot e -> PreNot (normalize e)
       Cast t e -> Cast t (normalize e)
       BinOp e1 op e2 -> BinOp (normalize e1) op (normalize e2)
       Cond g e1 e2 -> Cond (normalize g) (normalize e1) (normalize e2)

       -- left undefined:
       Lambda lParams lExp -> undefined
       InstanceOf e refType -> undefined

       -- unchanged:
       e' -> e'


   registerInvocation (methodid,k) = worker
      where
      worker [] =  [ (methodid, [k]) ]
      worker ((mx,z):therest)
          | methodid == mx = (mx, k:z) : therest
          | otherwise      = (mx,z) : worker therest


   countCallVars calls e = case e of
       -- base case
       ename@(ExpName n) ->
          let
          k = getCallCount__ ename
          methodid = getMethodId__ ename
          in
          if k >= 0 then registerInvocation (methodid,k) calls
                    else calls

       -- recursions
       PrePlus e      -> countCallVars calls e
       PreMinus e     -> countCallVars calls e
       PreBitCompl e  -> countCallVars calls e
       PreNot e       -> countCallVars calls e
       Cast t e       -> countCallVars calls e
       BinOp e1 op e2 ->  countCallVars (countCallVars calls e1) e2
       Cond g e1 e2   -> countCallVars (countCallVars (countCallVars calls g) e1) e2

       -- left undefined:
       Lambda lParams lExp  -> undefined
       InstanceOf e refType -> undefined

       -- ignored (should not contian method calls):
       e' -> calls

-- | Get all method names in a Java source file.
parseMethodIds :: FilePath -> IO [String]
parseMethodIds src = do
  decls <- parseDecls src
  -- get the names of all the class methods
  return $ map (\x -> case x of Ident s -> s) (getMethodIds decls)

parseDeclsRaw :: String -> IO [TypeDecl]
parseDeclsRaw src = do
  compilationUnit <- parseJavaRaw src
  return $ getDecls compilationUnit
  where
    -- parse a Java source file, and extracts the necessary information from the compilation unit
    parseJavaRaw :: String -> IO CompilationUnit
    parseJavaRaw source =
      case parser compilationUnit source of
          Left parseError -> error (show parseError)
          Right compUnit  -> return compUnit

-- | Get all the class declarations in the Java source file.
parseDecls :: FilePath -> IO [TypeDecl]
parseDecls src = do
  compilationUnit <- parseJava src
  return $ getDecls compilationUnit
  where
    -- parse a Java source file, and extracts the necessary information from the compilation unit
    parseJava :: FilePath -> IO CompilationUnit
    parseJava s = do
      -- Get the source code
      source <- readFile s
      -- Parse the source code
      case parser compilationUnit source of
          Left parseError -> error (show parseError)
          Right compUnit  -> return compUnit

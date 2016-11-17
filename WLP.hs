module WLP where

import Language.Java.Syntax
import Language.Java.Lexer
import Language.Java.Parser
import Data.Maybe
import Data.List

import Folds
import Verifier
import Substitute
import HelperFunctions
import Settings

    

-- | A type for the inherited attribute
data Inh = Inh {acc     :: Exp -> Exp,  -- The accumulated transformer of the current block up until the current statement
                br      :: Exp -> Exp,  -- The accumulated transformer up until the last loop (this is used when handling break statements etc.)
                catch   :: Maybe ([Catch], Bool), -- The catches when executing a block in a try statement, and a Bool indicating wether there is a finally-block
                env     :: TypeEnv,     -- The type environment for typing expressions
                decls   :: [TypeDecl] -- Class declarations
                }
            
           
-- | A type synonym for the synthesized attributea
type Syn = (Exp -> Exp, -- The wlp transformer
            TypeEnv)    -- The type environment

-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh       = foldr (\b (r, env') -> wlpBlock (inh {acc = r, env = env'}) b) (acc inh, envBlock bs (env inh)) bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc. The type environment is build from the left, so it has to be done seperately.
    fIfThen e s1                    = fIfThenElse e s1 (const (id, [])) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh         = ((\q -> (e &* fst (s1 inh) q) |* (neg e &* fst (s2 inh) q)) . acc inh, env inh)
    fWhile e s inh                  = ((\q -> if unsafeIsTrue (((inv &* neg e) `imp` q) &* ((inv &* e) `imp` fst (s inh) inv) &* ((inv &* e) `imp` fst (s inh {br = const q}) true)) then inv else (neg e &* q)) . acc inh, env inh)
    fBasicFor init me incr s inh    = let loop = fst (fWhile (fromMaybeGuard me) (\inh' -> s (inh {acc = fst (wlp' inh' (incrToStmt incr))})) inh) in wlp' (inh {acc = loop}) (initToStmt init)
    fEnhancedFor                    = error "EnhancedFor"
    fEmpty inh                      = (acc inh, env inh) -- Empty does nothing, but still passes control to the next statement
    fExpStmt e inh                  = snd $ foldExp wlpExpAlgebra e inh
    fAssert e _ inh                 = ((e &*) . acc inh, env inh)
    fSwitch e bs inh                = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) (inh {acc = id, br = acc inh})
    fDo s e inh                     = (fst (s (inh {acc = fst (fWhile e s inh)})), env inh) -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ inh                    = (br inh, env inh) -- wlp of the breakpoint. Control is passed to the statement after the loop
    fContinue _ inh                 = (id, env inh)     -- at a continue statement it's as if the body of the loop is fully executed
    fReturn me inh                  = case me of
                                        Nothing -> (id, env inh) -- Return ignores acc, as it terminates the method
                                        Just e  -> fExpStmt (Assign (NameLhs (Name [Ident "return"])) EqualA e) inh -- We treat "return e" as an assignment to the variable return
                                                            
    fSynchronized _                 = fStmtBlock
    fThrow e inh                    = case catch inh of
                                        Nothing      -> ((\q -> q &* throwException e), env inh) -- acc is ignored, as the rest of the block is not executed
                                        Just (cs, f) -> (maybe (if f then id else (\q -> q &* throwException e), env inh) (flip fStmtBlock (inh {acc = id, catch = Nothing})) (getCatch (decls inh) (env inh) e cs))
    fTry b cs f inh                 = let (r, env') = (fStmtBlock b (inh {acc = id, catch = Just (cs, isJust f)})) in (r . maybe (acc inh) (fst . flip fStmtBlock (inh {env = env'})) f, env inh) -- The finally-block is always executed
    fLabeled _ s                    = s
    
    -- Helper functions
    
    -- A block also keeps track of the types of declared variables
    wlpBlock :: Inh -> BlockStmt -> Syn
    wlpBlock inh b  = case b of
                        BlockStmt s            -> wlp' inh s
                        LocalClass _           -> (acc inh, env inh)
                        LocalVars mods t vars  -> foldr (\v (r, env') -> (wlpDeclAssignment t (inh {acc = r, env = env'}) v, env')) (acc inh, env inh) vars
                                                        
    -- Adds declarations within a block to a type environment
    envBlock :: [BlockStmt] -> TypeEnv -> TypeEnv
    envBlock bs env = foldl f env bs 
        where f env (LocalVars mods t vars)                                         = foldr (\v env' -> (varName v, t):env') env vars
              f env (BlockStmt (BasicFor (Just (ForLocalVars mods t vars)) _ _ _))  = foldr (\v env' -> (varName v, t):env') env vars
              f env _                                                               = env
              varName (VarDecl (VarId id) _) = Name [id]
                
    -- wlp of a var declaration that also assigns a value. Declaring without assignment assigns the default value
    wlpDeclAssignment :: Type -> Inh -> VarDecl -> Exp -> Exp
    wlpDeclAssignment t inh (VarDecl (VarId ident) Nothing) = case t of 
                                                                PrimType _ -> substVar (env inh) (decls inh) (NameLhs (Name [ident])) (getInitValue t) . acc inh
                                                                -- We don't initialize ref types to null, because we want to keep pointer information
                                                                RefType _ -> acc inh 
    wlpDeclAssignment t inh (VarDecl (VarId ident) (Just (InitExp e)))  = substVar (env inh) (decls inh) (NameLhs (Name [ident])) e . acc inh
                        
    --inv = true -- for simplicity, "True" is used as an invariant for now
    inv = case parser Language.Java.Parser.exp invariant of
            Right e -> e
            _       -> error "syntax error in post-condition"
    
    
    -- Converts initialization code of a for loop to a statement
    initToStmt :: Maybe ForInit -> Stmt
    initToStmt Nothing                              = Empty
    initToStmt (Just (ForInitExps es))              = StmtBlock (Block (map (BlockStmt . ExpStmt) es))
    initToStmt (Just (ForLocalVars mods t vars))    = StmtBlock (Block [LocalVars mods t vars])
    
    -- Replaces an absent guard with "True"
    fromMaybeGuard :: Maybe Exp -> Exp
    fromMaybeGuard Nothing  = true
    fromMaybeGuard (Just e) = e
    
    -- Converts increment code of a for loop to a statement
    incrToStmt :: Maybe [Exp] -> Stmt
    incrToStmt Nothing   = Empty
    incrToStmt (Just es) = StmtBlock (Block (map (BlockStmt . ExpStmt) es))
    
    -- Converts a switch into nested if-then-else statements. The switch is assumed to be non-trivial.
    desugarSwitch :: Exp -> [SwitchBlock] -> (Exp, Stmt, Stmt)
    desugarSwitch e [SwitchBlock l bs]       = case l of
                                                    SwitchCase e' -> (BinOp e Equal e', StmtBlock (Block bs), Empty)
                                                    Default -> (true, StmtBlock (Block bs), Empty)
    desugarSwitch e (SwitchBlock l bs:sbs)   = case l of
                                                    SwitchCase e' -> (BinOp e Equal e', StmtBlock (Block bs), sbscode)
                                                    Default -> (true, StmtBlock (Block (bs ++ [BlockStmt sbscode])), sbscode)
        where sbscode = let (e, s1, s2) = desugarSwitch e sbs in IfThenElse e s1 s2
        
throwException :: Exp -> Exp
throwException e = false
    
getCatch :: [TypeDecl] -> TypeEnv -> Exp -> [Catch] -> Maybe Block
getCatch decls env e []             = Nothing
getCatch decls env e (Catch p b:cs) = if catches decls env p e then Just b else getCatch decls env e cs

-- Checks whether a catch block catches a certain error
catches :: [TypeDecl] -> TypeEnv -> FormalParam -> Exp -> Bool
catches decls env (FormalParam _ t _ _) e = t == RefType (ClassRefType (ClassType [(Ident "Exception", [])])) || 
                                              case e of
                                                ExpName name -> lookupType decls env name == t
                                                InstanceCreation _ t' _ _ -> t == RefType (ClassRefType t')
    
-- | The algebra that defines the wlp transformer for expressions with side effects
--   The first attribute is the expression itself (this is passed to handle substitutions in case of assignments)
wlpExpAlgebra :: ExpAlgebra (Inh -> (Exp, Syn))
wlpExpAlgebra = (fLit, fClassLit, fThis, fThisClass, fInstanceCreation, fQualInstanceCreation, fArrayCreate, fArrayCreateInit, fFieldAccess, fMethodInv, fArrayAccess, fExpName, fPostIncrement, fPostDecrement, fPreIncrement, fPreDecrement, fPrePlus, fPreMinus, fPreBitCompl, fPreNot, fCast, fBinOp, fInstanceOf, fCond, fAssign, fLambda, fMethodRef) where
    fLit lit inh = (Lit lit, (acc inh, env inh))
    fClassLit = undefined
    fThis = undefined
    fThisClass = undefined
    fInstanceCreation typeArgs t args mBody inh         = (InstanceCreation typeArgs t args mBody, (acc inh, env inh))
    fQualInstanceCreation e typeArgs t args mBody inh   = (QualInstanceCreation (getExp e inh) typeArgs t args mBody, (getTrans e inh, env inh))
    fArrayCreate t dimLengths dim inh                   = (ArrayCreate t (map (flip getExp inh) dimLengths) dim, (acc inh, env inh))
    fArrayCreateInit t dim init inh                     = (ArrayCreateInit t dim init, (acc inh, env inh))
    fFieldAccess fieldAccess inh                        = case fieldAccess of
                                                            PrimaryFieldAccess e (Ident field) -> (ArrayAccess (ArrayIndex (getExp (foldExp wlpExpAlgebra e) inh) [Lit (String field)]), (acc inh, env inh)) 
                                                            _ -> error "fieldaccess"
    fMethodInv                                          = error "method call"
    fArrayAccess (ArrayIndex a i) inh                   = case catch inh of
                                                            Nothing      -> (arrayAccess a i, (acc inh, env inh))
                                                            Just (cs, f) -> (arrayAccess a i, (arrayAccessWlp a i inh, env inh))
    
    fExpName name inh                                   = (ExpName name, (acc inh, env inh))
    -- x++ increments x but evaluates to the original value
    fPostIncrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (exp, (acc inh, env inh))
    fPostDecrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (exp, (acc inh, env inh))
    -- ++x increments x and evaluates to the new value of x
    fPreIncrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Add (Lit (Int 1)), (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (BinOp exp Add (Lit (Int 1)), (acc inh, env inh))
    fPreDecrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Rem (Lit (Int 1)), (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . acc inh, env inh))
                                                            exp  -> (BinOp exp Rem (Lit (Int 1)), (acc inh, env inh))
    fPrePlus e inh                                      = (getExp e inh, (acc inh, env inh))
    fPreMinus e inh                                     = (PreMinus $ getExp e inh, (acc inh, env inh))
    fPreBitCompl e inh                                  = (PreBitCompl $ getExp e inh, (acc inh, env inh))
    fPreNot e inh                                       = (PreNot $ getExp e inh, (acc inh, env inh))
    fCast t e inh                                       = (getExp e inh, (acc inh, env inh))
    fBinOp e1 op e2 inh                                 = (BinOp (getExp e1 inh) op (getExp e2 inh), (getTrans e1 (inh {acc = getTrans e2 inh}), env inh)) 
    fInstanceOf                                         = error "TODO: instanceOf"
    fCond g e1 e2 inh                                   = (Cond (getExp g inh) (getExp e1 inh) (getExp e2 inh), (getTrans g (inh {acc = id}) . (\q -> (getExp g inh &* getTrans e1 (inh {acc = id}) q) |* (neg (getExp g inh) &* getTrans e2 (inh {acc = id}) q)) . acc inh, env inh))
    fAssign lhs op e inh                                = let rhs = desugarAssign lhs op (getExp e inh) 
                                                          in  (rhs, (substVar (env inh) (decls inh) lhs rhs . getTrans e inh, env inh))
    fLambda                                             = error "lambda"
    fMethodRef                                          = error "method reference"
                            
    -- gets the transformer for array access (as array access may throw an error)
    arrayAccessWlp :: Exp -> [Exp] -> Inh -> Exp -> Exp
    arrayAccessWlp a i inh q =  case catch inh of
                                    Nothing      -> q
                                    Just (cs, f) -> let e = InstanceCreation [] (ClassType [(Ident "ArrayIndexOutOfBoundsException", [])]) [] Nothing
                                                    in Cond (foldr (\(i, l) e -> e &* (BinOp i LThan l)) true (zip i (dimLengths a))) q ((maybe (if f then q else q &* throwException e)) (\b -> fst (wlp' (inh {acc = id, catch = Nothing}) (StmtBlock b)) q) (getCatch (decls inh) (env inh) e cs))
                                
    dimLengths a = case a of
                    ArrayCreate t exps dim          -> exps
                    _                               -> map (\n -> MethodInv (MethodCall (Name [Ident "length"]) [a, (Lit (Int n))])) [0..]

-- | Gets the expression attribute
getExp :: (Inh -> (Exp, Syn)) -> Inh -> Exp
getExp f inh = let (e, _) = f inh in e

-- | Gets the transformer attribute
getTrans :: (Inh -> (Exp, Syn)) -> Inh -> Exp -> Exp
getTrans f inh = let (_, (trans, _)) = f inh in trans

-- | Gets the typeEnv attribute
getEnv :: (Inh -> (Exp, Syn)) -> Inh -> TypeEnv
getEnv f inh = let (_, (_, env)) = f inh in env


-- | Creates an array that represents an object
{- makeObjectArray :: [TypeDecl] -> ClassType -> [Argument] -> (Maybe ClassBody) -> Exp
makeObjectArray decls t = makeObjectArray' (getDecl t decls)
    where
        makeObjectArray' :: ClassDecl -> [Argument] -> (Maybe ClassBody) -> Exp
        makeObjectArray' (ClassDecl _ _ _ _ _ (ClassBody decls)) args mbody = initObj decls
        
        -- Gets the class declaration that matches a given type
        getDecl :: ClassType -> [TypeDecl] -> ClassDecl
        getDecl t@(ClassType [(ident, typeArgs)]) (x:xs)    = case x of
                                                                ClassTypeDecl decl@(ClassDecl _ ident' _ _ _ _) -> if ident == ident' then decl else getDecl t xs
                                                                _ -> getDecl t xs
        getDecl _ _ = error "nested class"
        
        -- Initializes the member variables (without calling the constructor etc.)
        initObj :: [Decl] -> Exp
        initObj decls = foldr (\(ident, e) arr -> Assign (ArrayLhs (ArrayIndex arr [Lit (String ident)])) EqualA e) (ArrayCreate (RefType (ClassRefType (ClassType []))) [Lit (Int (toEnum (length decls)))] 0) (getFields decls)
        
        getFields :: [Decl] -> [(String, Exp)]
        getFields = foldr f []
            where
                f (MemberDecl (FieldDecl mods t (v : vars))) = f (MemberDecl (FieldDecl mods t vars)) . f' (MemberDecl (FieldDecl mods t [v]))
                f _ = id
                f' (MemberDecl (FieldDecl _ t [(VarDecl ident mInit)])) = case mInit of
                                                                            Nothing                     -> ((getId ident, getInitValue t) :)
                                                                            Just (InitExp e)            -> ((getId ident, e) :)
                                                                            Just (InitArray arrayInit)  -> ((getId ident, ArrayCreateInit t (getDims ident) arrayInit) :)
                getId (VarId (Ident id)) = id
                getId (VarDeclArray id) = getId id
                getDims (VarId id) = 0
                getDims (VarDeclArray id) = getDims id + 1 -}
                
-- Initializes the heap
initHeap :: Exp
initHeap = Assign (NameLhs (Name [Ident "<heap>"])) EqualA (Lit Null) --(ArrayCreate objectType [Lit (Int 10000)] 0)

-- gets a value from the heap
--getFromHeap :: Name -> Exp
--getFromHeap (Name idents) = foldl (\e (Ident id) -> ArrayAccess (ArrayIndex e [Lit (String id)])) heap idents
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: [TypeDecl] -> Stmt -> Exp -> Exp
wlp decls = fst . (wlp' (Inh id id Nothing [] decls))

-- | wlp with a given type environment
wlpWithEnv :: [TypeDecl] -> TypeEnv -> Stmt -> Exp -> Exp
wlpWithEnv decls env = fst . (wlp' (Inh id id Nothing env decls))

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Syn
wlp' inh s = foldStmt wlpStmtAlgebra s inh


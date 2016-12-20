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
data Inh = Inh {acc     :: Exp -> Exp,              -- The accumulated transformer of the current block up until the current statement
                br      :: Exp -> Exp,              -- The accumulated transformer up until the last loop (this is used when handling break statements etc.)
                catch   :: Maybe ([Catch], Bool),   -- The catches when executing a block in a try statement, and a Bool indicating wether there is a finally-block
                env     :: TypeEnv,                 -- The type environment for typing expressions
                decls   :: [TypeDecl],              -- Class declarations
                calls   :: CallCount,               -- The number of recursive calls per method
                ret     :: Maybe Ident,             -- The name of the return variable when handling a method call
                object  :: Maybe Exp,               -- The object the method is called from when handling a method call
                varNr   :: Int                      -- Used for creating unique variable names
                }
            
           
-- | A type synonym for the synthesized attributea
type Syn = (Exp -> Exp, -- The wlp transformer
            TypeEnv)    -- The type environment

-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh       = fst $ foldr (\b ((r, env'), varNr') -> (wlpBlock (inh {acc = r, env = env', varNr = varNr'}) b, varNr' + 1)) ((acc inh, envBlock bs (env inh)), varNr inh) bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc. The type environment is build from the left, so it has to be done seperately.
    fIfThen e s1                    = fIfThenElse e s1 (const (id, [])) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh         = let e' = getExp (foldExp wlpExpAlgebra e) inh 
                                          eTrans = getTrans (foldExp wlpExpAlgebra e) inh {acc = id} -- The guard might also have side effects
                                      in ((\q -> (e' &* eTrans (fst (s1 inh {acc = id}) q)) |* (neg e' &* eTrans (fst (s2 inh {acc = id}) q))) . acc inh, env inh)
    fWhile e s inh                  = let e' = getExp (foldExp wlpExpAlgebra e) inh 
                                      in ((\q -> unrollLoop nrOfUnroll e' (getTrans (foldExp wlpExpAlgebra e) inh {acc = id}) (fst (s (inh {acc = id, br = const q}))) q) . acc inh, env inh)
    fBasicFor init me incr s inh    = let loop = fst (fWhile (fromMaybeGuard me) (\inh' -> s (inh {acc = fst (wlp' inh' (incrToStmt incr))})) inh) in wlp' (inh {acc = loop}) (initToStmt init)
    fEnhancedFor                    = error "EnhancedFor"
    fEmpty inh                      = (acc inh, env inh) -- Empty does nothing, but still passes control to the next statement
    fExpStmt e inh                  = snd $ foldExp wlpExpAlgebra e inh
    fAssert e _ inh                 = let e' = getExp (foldExp wlpExpAlgebra e) inh 
                                      in ((e' &*) . getTrans (foldExp wlpExpAlgebra e) inh, env inh)
    fSwitch e bs inh                = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) (inh {acc = id, br = acc inh})
    fDo s e inh                     = (fst (s (inh {acc = fst (fWhile e s inh)})), env inh) -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ inh                    = (br inh, env inh) -- wlp of the breakpoint. Control is passed to the statement after the loop
    fContinue _ inh                 = (id, env inh)     -- at a continue statement it's as if the body of the loop is fully executed
    fReturn me inh                  = case me of
                                        Nothing -> (id, env inh) -- Return ignores acc, as it terminates the method
                                        Just e  -> fExpStmt (Assign (NameLhs (Name [fromJust (ret inh)])) EqualA e) (inh {acc = id}) -- We treat "return e" as an assignment to a variable specifically created to store the return value in
                                                            
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
    wlpDeclAssignment t inh (VarDecl (VarId ident) (Just (InitExp e)))  = getTrans (foldExp wlpExpAlgebra (Assign (NameLhs (Name [ident])) EqualA e)) inh 
              
    -- Unrolls a while-loop a finite amount of times
    unrollLoop :: Int -> Exp -> (Exp -> Exp) -> (Exp -> Exp) -> Exp -> Exp
    unrollLoop 0 g gTrans _ q           = (neg g `imp` gTrans q)
    unrollLoop n g gTrans bodyTrans q   = (neg g `imp` gTrans q) &* (g `imp` gTrans (bodyTrans (unrollLoop (n - 1) g gTrans bodyTrans q)))
    
    
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
    desugarSwitch e [SwitchBlock l bs]          = case l of
                                                    SwitchCase e'   -> (BinOp e Equal e', StmtBlock (Block bs), Empty)
                                                    Default         -> (true, StmtBlock (Block bs), Empty)
    desugarSwitch e sbs@(SwitchBlock l bs:sbs') = case l of
                                                    SwitchCase e'   -> (BinOp e Equal e', StmtBlock (switchBlockToBlock sbs), otherCases)
                                                    Default         -> (true, StmtBlock (switchBlockToBlock sbs), otherCases)
        where otherCases = let (e', s1, s2) = desugarSwitch e sbs' in IfThenElse e' s1 s2
        
    -- Gets the statements from a switch statement
    switchBlockToBlock :: [SwitchBlock] -> Block
    switchBlockToBlock []                       = Block []
    switchBlockToBlock (SwitchBlock l bs:sbs)   = case switchBlockToBlock sbs of
                                                    Block b -> Block (bs ++ b)
        
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
    fLit lit inh                                        = (Lit lit, (acc inh, env inh))
    fClassLit mType inh                                 = (ClassLit mType, (acc inh, env inh))
    fThis inh                                           = (fromJust (object inh), (acc inh, env inh))
    fThisClass name inh                                 = (ThisClass name, (acc inh, env inh))
    fInstanceCreation typeArgs t args mBody inh         = case args of
                                                            [ExpName (Name [Ident "#"])]    -> (InstanceCreation typeArgs t args mBody, (acc inh, env inh)) -- '#' indicates we already called the constructor method using the correct arguments
                                                            _                               -> -- Create a var, assign a new instance to var, then call the constructor method on var
                                                                    let (var, invocation) = (Name [getReturnVar inh invocation], MethodCall (Name [getReturnVar inh invocation, Ident ("#" ++ getClassName t)]) args) 
                                                                    in  (ExpName var, (substVar (env inh) (decls inh) (NameLhs var) (InstanceCreation typeArgs t [ExpName (Name [Ident "#"])] mBody) . getTrans (fMethodInv invocation) inh {acc = id, object = Just (ExpName var)} . acc inh, env inh))
    fQualInstanceCreation e typeArgs t args mBody inh   = (QualInstanceCreation (getExp e inh) typeArgs t args mBody, (getTrans e inh, env inh))
    fArrayCreate t dimLengths dim inh                   = (ArrayCreate t (map (flip getExp inh) dimLengths) dim, (acc inh, env inh))
    fArrayCreateInit t dim init inh                     = (ArrayCreateInit t dim init, (acc inh, env inh))
    fFieldAccess fieldAccess inh                        = (ExpName (foldFieldAccess inh fieldAccess), (acc inh, env inh))
    fMethodInv invocation inh                           = case invocation of
                                                            MethodCall (Name [Ident "*assume"]) [e] -> (Lit Null, (if e == false then const true else imp e, env inh))
                                                            _   -> (ExpName (Name [getReturnVar inh invocation]), 
                                                                   (if getCallCount (calls inh) (invocationToId invocation) >= nrOfUnroll
                                                                   then const true
                                                                   else fst (wlp' (inh {acc = id, calls = incrCallCount (calls inh) (invocationToId invocation), ret = Just (getReturnVar inh invocation), object = getObject inh invocation}) (inlineMethod inh invocation)) . acc inh, env inh))
    fArrayAccess (ArrayIndex a i) inh                   = case catch inh of
                                                            Nothing      -> (arrayAccess a i, (getTrans (foldExp wlpExpAlgebra a) inh, env inh))
                                                            Just (cs, f) -> (arrayAccess a i, (getTrans (foldExp wlpExpAlgebra a) inh {acc = id} . arrayAccessWlp a i inh, env inh))
    
    fExpName name inh                                   = (ExpName name, (acc inh, env inh))
    -- x++ increments x but evaluates to the original value
    fPostIncrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . getTrans e inh, env inh))
                                                            exp  -> (exp, (getTrans e inh, env inh))
    fPostDecrement e inh                                = case getExp e inh of
                                                            var@(ExpName name) -> (var, (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . getTrans e inh, env inh))
                                                            exp  -> (exp, (getTrans e inh, env inh))
    -- ++x increments x and evaluates to the new value of x
    fPreIncrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Add (Lit (Int 1)), (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . getTrans e inh, env inh))
                                                            exp  -> (BinOp exp Add (Lit (Int 1)), (getTrans e inh, env inh))
    fPreDecrement e inh                                 = case getExp e inh of
                                                            var@(ExpName name) -> (BinOp var Rem (Lit (Int 1)), (substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . getTrans e inh, env inh))
                                                            exp  -> (BinOp exp Rem (Lit (Int 1)), (getTrans e inh, env inh))
    fPrePlus e inh                                      = (getExp e inh, (getTrans e inh, env inh))
    fPreMinus e inh                                     = (PreMinus $ getExp e inh, (getTrans e inh, env inh))
    fPreBitCompl e inh                                  = (PreBitCompl $ getExp e inh, (getTrans e inh, env inh))
    fPreNot e inh                                       = (PreNot $ getExp e inh, (getTrans e inh, env inh))
    fCast t e inh                                       = (getExp e inh, (getTrans e inh, env inh))
    fBinOp e1 op e2 inh                                 = (BinOp (getExp e1 inh) op (getTrans e1 (inh {acc = id}) (getExp e2 inh)), (getTrans e1 (inh {acc = getTrans e2 inh}), env inh)) -- Side effects of the first expression are applied before the second is evaluated, so we have to apply the transformer
    fInstanceOf                                         = error "instanceOf"
    fCond g e1 e2 inh                                   = (Cond (getExp g inh) (getExp e1 inh) (getExp e2 inh), (getTrans g (inh {acc = id}) . (\q -> (getExp g inh &* getTrans e1 (inh {acc = id}) q) |* (neg (getExp g inh) &* getTrans e2 (inh {acc = id}) q)) . acc inh, env inh))
    fAssign lhs op e inh                                = let lhs' = foldLhs inh lhs
                                                              rhs' = desugarAssign lhs' op (getExp e inh) 
                                                          in  (rhs', (getTrans e inh {acc = id} . substVar (env inh) (decls inh) lhs' rhs' . acc inh, env inh))
    fLambda                                             = error "lambda"
    fMethodRef                                          = error "method reference"
                            
    -- gets the transformer for array access (as array access may throw an error)
    arrayAccessWlp :: Exp -> [Exp] -> Inh -> Exp -> Exp
    arrayAccessWlp a i inh q =  case catch inh of
                                    Nothing      -> q
                                    Just (cs, f) -> let e = InstanceCreation [] (ClassType [(Ident "ArrayIndexOutOfBoundsException", [])]) [] Nothing
                                                    in Cond (foldr (\(i, l) e -> e &* (BinOp i LThan l) &* (BinOp i GThanE (Lit (Int 0)))) true (zip i (dimLengths a))) q ((maybe (if f then q else q &* throwException e)) (\b -> fst (wlp' (inh {acc = id, catch = Nothing}) (StmtBlock b)) q) (getCatch (decls inh) (env inh) e cs))
                                
    dimLengths a = case a of
                    ArrayCreate t exps dim          -> exps
                    _                               -> map (\n -> MethodInv (MethodCall (Name [Ident "*length"]) [a, (Lit (Int n))])) [0..]
                    
    -- Inlines a methodcall. This creates a variable to store the return value in
    inlineMethod :: Inh -> MethodInvocation -> Stmt
    inlineMethod inh invocation = StmtBlock (Block (getParams inh invocation ++ [BlockStmt (getBody inh invocation)])) where
        -- Gets the body of the method
        getBody :: Inh -> MethodInvocation -> Stmt
        getBody inh (MethodCall name _)             = case getMethod (decls inh) (getMethodId name) of
                                                        Nothing -> ExpStmt $ MethodInv (MethodCall (Name [Ident "*assume"]) [false])
                                                        Just s  -> s
        getBody inh (PrimaryMethodCall _ _ id _)    = case getMethod (decls inh) id of
                                                        Nothing -> ExpStmt $ MethodInv (MethodCall (Name [Ident "*assume"]) [false])
                                                        Just s  -> s
        getBody inh _ = undefined
        -- Assigns the supplied parameter values to the parameter variables
        getParams :: Inh -> MethodInvocation -> [BlockStmt]
        getParams inh (MethodCall name args)            = case getMethodParams (decls inh) (getMethodId name) of 
                                                            Nothing     -> []
                                                            Just params -> zipWith assignParam params args
        getParams inh (PrimaryMethodCall _ _ id args)   = case getMethodParams (decls inh) id of 
                                                            Nothing     -> []
                                                            Just params -> zipWith assignParam params args
        getParams inh _ = undefined
        -- Creates an assignment statement to a parameter variable
        assignParam :: FormalParam -> Exp -> BlockStmt
        assignParam (FormalParam mods t _ varId) e = LocalVars mods t [VarDecl varId (Just (InitExp e))]
    
    -- Gets the variable that represents the return value of the method
    getReturnVar :: Inh -> MethodInvocation -> Ident
    getReturnVar inh invocation = Ident (makeReturnVarName (invocationToId invocation) ++ show (varNr inh) ++ "," ++ show (getCallCount (calls inh) (invocationToId invocation)))
    
    -- Gets the object a method is called from
    getObject :: Inh -> MethodInvocation -> Maybe Exp
    getObject inh (MethodCall name _)   | length (fromName name) > 1    = Just (ExpName (Name (take (length (fromName name) - 1) (fromName name))))
                                    | otherwise                         = Nothing
    getObject inh (PrimaryMethodCall e _ _ _)                           = case e of
                                                                            This -> object inh
                                                                            _    -> Just e
    getObject inh _                                                     = undefined
    
    -- Gets the name of the class as a string from the type
    getClassName :: ClassType -> String
    getClassName (ClassType xs) = let Ident s = fst (last xs) in s
    
    -- Gets the return type of a method
    getType :: Inh -> MethodInvocation -> Maybe Type
    getType inh invocation = getMethodType (decls inh) (invocationToId invocation)
    
    -- Gets the method Id from an invocation
    invocationToId :: MethodInvocation -> Ident
    invocationToId (MethodCall name _) = getMethodId name
    invocationToId (PrimaryMethodCall _ _ id _) = id
    invocationToId _ = undefined
    
    -- Folds the expression part of an lhs
    foldLhs :: Inh -> Lhs -> Lhs
    foldLhs inh lhs  = case lhs of
                            FieldLhs (PrimaryFieldAccess e id)  -> case getExp (foldExp wlpExpAlgebra e) inh of
                                                                    ExpName name    -> NameLhs (Name (fromName name ++ [id]))
                                                                    _               -> error "foldFieldAccess"
                            ArrayLhs (ArrayIndex e i)           -> ArrayLhs (ArrayIndex (getExp (foldExp wlpExpAlgebra e) inh) (map (\e -> getExp (foldExp wlpExpAlgebra e) inh) i))
                            lhs'                                -> lhs'
    
    -- Folds the expression part of a fieldaccess and simplifies it to a name
    foldFieldAccess :: Inh -> FieldAccess -> Name
    foldFieldAccess inh fieldAccess  = case fieldAccess of
                                            PrimaryFieldAccess e id     -> case getExp (foldExp wlpExpAlgebra e) inh of
                                                                                ExpName name    -> Name (fromName name ++ [id])
                                                                                x               -> error ("foldFieldAccess: " ++ show x ++ show id)
                                            SuperFieldAccess id         -> foldFieldAccess inh (PrimaryFieldAccess (fromJust (object inh)) id)
                                            ClassFieldAccess name id    -> Name (fromName name ++ [id])

-- | Gets the expression attribute
getExp :: (Inh -> (Exp, Syn)) -> Inh -> Exp
getExp f inh = let (e, _) = f inh in e

-- | Gets the transformer attribute
getTrans :: (Inh -> (Exp, Syn)) -> Inh -> Exp -> Exp
getTrans f inh = let (_, (trans, _)) = f inh in trans

-- | Gets the typeEnv attribute
getEnv :: (Inh -> (Exp, Syn)) -> Inh -> TypeEnv
getEnv f inh = let (_, (_, env)) = f inh in env
    
-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: [TypeDecl] -> Stmt -> Exp -> Exp
wlp decls = fst . (wlp' (Inh id id Nothing [] decls [] Nothing Nothing 0))

-- | wlp with a given type environment
wlpWithEnv :: [TypeDecl] -> TypeEnv -> Stmt -> Exp -> Exp
wlpWithEnv decls env = fst . (wlp' (Inh id id Nothing env decls [] Nothing Nothing 0))

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Syn
wlp' inh s = foldStmt wlpStmtAlgebra s inh


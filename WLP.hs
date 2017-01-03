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
                object  :: Maybe Exp                -- The object the method is called from when handling a method call
                }
            
           
-- | A type synonym for the synthesized attributea
type Syn = Exp -> Exp -- The wlp transformer

-- | The algebra that defines the wlp transformer for statements
--   The synthesized attribute is the resulting transformer. 
--   Statements that pass control to the next statement have to explicitly combine their wlp function with the accumulated function, as some statements (e.g. break) ignore the accumulated function.
wlpStmtAlgebra :: StmtAlgebra (Inh -> Syn)
wlpStmtAlgebra = (fStmtBlock, fIfThen, fIfThenElse, fWhile, fBasicFor, fEnhancedFor, fEmpty, fExpStmt, fAssert, fSwitch, fDo, fBreak, fContinue, fReturn, fSynchronized, fThrow, fTry, fLabeled) where
    fStmtBlock (Block bs) inh       = foldr (\b r -> wlpBlock (inh {acc = r, env = envBlock bs (env inh)}) b) (acc inh) bs -- The result of the last block-statement will be the accumulated transformer for the second-last etc. The type environment is build from the left, so it has to be done seperately.
    fIfThen e s1                    = fIfThenElse e s1 (const id) -- if-then is just an if-then-else with an empty else-block
    fIfThenElse e s1 s2 inh         = let (e', trans) = foldExp wlpExpAlgebra e inh {acc = id}
                                          var = getVar
                                      in trans . substVar' inh var e' . (\q -> (ExpName (Name [var]) &* s1 inh {acc = id} q) |* (neg (ExpName (Name [var])) &* s2 inh {acc = id} q)) . acc inh
    fWhile e s inh                  = let (e', trans) = foldExp wlpExpAlgebra e inh {acc = id}
                                          var = getVar
                                      in (\q -> unrollLoop inh nrOfUnroll e' trans (s (inh {acc = id, br = const q})) q) . acc inh
    fBasicFor init me incr s inh    = let loop = (fWhile (fromMaybeGuard me) (\inh' -> s (inh {acc = (wlp' inh {acc = id} (incrToStmt incr))})) inh) in wlp' (inh {acc = loop}) (initToStmt init)
    fEnhancedFor                    = error "EnhancedFor"
    fEmpty inh                      = (acc inh) -- Empty does nothing, but still passes control to the next statement
    fExpStmt e inh                  = snd $ foldExp wlpExpAlgebra e inh
    fAssert e _ inh                 = let (e', trans) = foldExp wlpExpAlgebra e inh {acc = id}
                                      in (trans . (e' &*) . acc inh)
    fSwitch e bs inh                = let (e', s1, s2) = desugarSwitch e bs in fIfThenElse e' (flip wlp' s1) (flip wlp' s2) (inh {acc = id, br = acc inh})
    fDo s e inh                     = s (inh {acc = fWhile e s inh}) -- Do is just a while with the statement block executed one additional time. Break and continue still have to be handled in this additional execution.
    fBreak _ inh                    = br inh -- wlp of the breakpoint. Control is passed to the statement after the loop
    fContinue _ inh                 = id     -- at a continue statement it's as if the body of the loop is fully executed
    fReturn me inh                  = case me of
                                        Nothing -> id -- Return ignores acc, as it terminates the method
                                        Just e  -> fExpStmt (Assign (NameLhs (Name [fromJust' "fReturn" (ret inh)])) EqualA e) (inh {acc = id}) -- We treat "return e" as an assignment to a variable specifically created to store the return value in
                                                            
    fSynchronized _                 = fStmtBlock
    fThrow e inh                    = case catch inh of
                                        Nothing      -> ((\q -> q &* throwException e)) -- acc is ignored, as the rest of the block is not executed
                                        Just (cs, f) -> (maybe (if f then id else (\q -> q &* throwException e)) (flip fStmtBlock (inh {acc = id, catch = Nothing})) (getCatch (decls inh) (env inh) e cs))
    fTry (Block bs) cs f inh        = let r = (fStmtBlock (Block bs) (inh {acc = id, catch = Just (cs, isJust f)})) in (r . maybe (acc inh) (flip fStmtBlock (inh {env = envBlock bs (env inh)})) f) -- The finally-block is always executed
    fLabeled _ s                    = s
    
    -- Helper functions
    
    -- A block also keeps track of the types of declared variables
    wlpBlock :: Inh -> BlockStmt -> Syn
    wlpBlock inh b  = case b of
                        BlockStmt s            -> wlp' inh s
                        LocalClass _           -> (acc inh)
                        LocalVars mods t vars  -> foldr (\v r -> (wlpDeclAssignment t (inh {acc = r}) v)) (acc inh) vars
                                                        
    -- Adds declarations within a block to a type environment
    envBlock :: [BlockStmt] -> TypeEnv -> TypeEnv
    envBlock bs env = foldl f env bs 
        where f env (LocalVars mods t vars)                                         = foldr (\v env' -> (varName v, t):env') env vars
              f env (BlockStmt (BasicFor (Just (ForLocalVars mods t vars)) _ _ s))  = foldr (\v env' -> (varName v, t):env') env vars
              f env _                                                               = env
              varName (VarDecl (VarId id) _) = Name [id]
                
    -- wlp of a var declaration that also assigns a value. Declaring without assignment assigns the default value
    wlpDeclAssignment :: Type -> Inh -> VarDecl -> Exp -> Exp
    wlpDeclAssignment t inh (VarDecl (VarId ident) Nothing) = case t of 
                                                                PrimType _ -> substVar (env inh) (decls inh) (NameLhs (Name [ident])) (getInitValue t) . acc inh
                                                                -- We don't initialize ref types to null, because we want to keep pointer information
                                                                RefType _ -> acc inh 
    wlpDeclAssignment t inh (VarDecl (VarId ident) (Just (InitExp e)))  = snd (foldExp wlpExpAlgebra (Assign (NameLhs (Name [ident])) EqualA e) inh)
    wlpDeclAssignment _ _ _ = error "ArrayCreateInit is not supported"
              
    -- Unrolls a while-loop a finite amount of times
    unrollLoop :: Inh -> Int -> Exp -> (Exp -> Exp) -> (Exp -> Exp) -> Exp -> Exp
    unrollLoop inh 0 g gTrans _             = let var = getVar
                                              in gTrans . substVar' inh var g . (neg (ExpName (Name [var])) `imp`)
    unrollLoop inh n g gTrans bodyTrans     = let var = getVar
                                              in gTrans . substVar' inh var g . (\q -> (ExpName (Name [var]) &* bodyTrans q) |* (neg (ExpName (Name [var])) &* unrollLoop inh (n-1) g gTrans bodyTrans q))
    
    
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
    fLit lit inh                                        = (Lit lit, (acc inh))
    fClassLit mType inh                                 = (ClassLit mType, (acc inh))
    fThis inh                                           = (fromJust' "fThis" (object inh), (acc inh))
    fThisClass name inh                                 = (ThisClass name, (acc inh))
    fInstanceCreation typeArgs t args mBody inh         = case args of
                                                            [ExpName (Name [Ident "#"])]    -> (InstanceCreation typeArgs t args mBody, acc inh) -- '#' indicates we already called the constructor method using the correct arguments
                                                            _                               -> -- Create a var, assign a new instance to var, then call the constructor method on var
                                                                    let varId = getReturnVar invocation
                                                                        var = Name [varId]
                                                                        invocation = MethodCall (Name [varId, Ident ("#" ++ getClassName t)]) args
                                                                    in  (ExpName var, (substVar (env inh) (decls inh) (NameLhs var) (InstanceCreation typeArgs t [ExpName (Name [Ident "#"])] mBody) . snd ((fMethodInv invocation) inh {acc = id}) . acc inh))
    fQualInstanceCreation e typeArgs t args mBody inh   = error "fQualInstanceCreation"
    fArrayCreate t dimLengths dim inh                   = (ArrayCreate t (map (\e -> fst (e inh)) dimLengths) dim, acc inh)
    fArrayCreateInit t dim init inh                     = error "ArrayCreateInit" -- (ArrayCreateInit t dim init, acc inh)
    fFieldAccess fieldAccess inh                        = (ExpName (foldFieldAccess inh fieldAccess), (acc inh))
    fMethodInv invocation inh                           = case invocation of
                                                            MethodCall (Name [Ident "*assume"]) [e] -> (false, (if e == false then const true else imp e)) -- *assume is the regular assume function
                                                            _   -> if getCallCount (calls inh) (invocationToId invocation) >= nrOfUnroll  -- Check the recursion depth
                                                                   then (undefined, const true) -- Recursion limit reached: we just assume the post codition will hold
                                                                   else let varId = getReturnVar invocation     
                                                                            callWlp = wlp' (inh {acc = id, calls = incrCallCount (calls inh) (invocationToId invocation), ret = Just varId, object = getObject inh invocation}) (inlineMethod inh invocation)
                                                                        in (ExpName (Name [varId]), (callWlp . acc inh))
    fArrayAccess (ArrayIndex a i) inh                   = (arrayAccess a i, snd ((foldExp wlpExpAlgebra a) inh {acc = id}) . arrayAccessWlp a i inh)
    
    fExpName name inh                                   = (editName inh name, acc inh)
    -- x++ increments x but evaluates to the original value
    fPostIncrement e inh                                = let (e', trans) = e inh in 
                                                          case e' of
                                                            var@(ExpName name) -> (var, substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . trans)
                                                            exp  -> (exp, trans)
    fPostDecrement e inh                                = let (e', trans) = e inh in
                                                          case e' of
                                                            var@(ExpName name) -> (var, substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . trans)
                                                            exp  -> (exp, trans)
    -- ++x increments x and evaluates to the new value of x
    fPreIncrement e inh                                 = let (e', trans) = e inh in 
                                                          case e' of
                                                            var@(ExpName name) -> (BinOp var Add (Lit (Int 1)), substVar (env inh) (decls inh) (NameLhs name) (BinOp var Add (Lit (Int 1))) . trans)
                                                            exp  -> (BinOp exp Add (Lit (Int 1)), trans)
    fPreDecrement e inh                                 = let (e', trans) = e inh in 
                                                          case e' of
                                                            var@(ExpName name) -> (BinOp var Rem (Lit (Int 1)), substVar (env inh) (decls inh) (NameLhs name) (BinOp var Rem (Lit (Int 1))) . trans)
                                                            exp  -> (BinOp exp Rem (Lit (Int 1)), trans)
    fPrePlus e inh                                      = let (e', trans) = e inh in (e', trans)
    fPreMinus e inh                                     = let (e', trans) = e inh in (PreMinus e', trans)
    fPreBitCompl e inh                                  = let (e', trans) = e inh in (PreBitCompl e', trans)
    fPreNot e inh                                       = let (e', trans) = e inh in (PreNot e', trans)
    fCast t e inh                                       = let (e', trans) = e inh in (e', trans)
    fBinOp e1 op e2 inh                                 = let (e1', trans1) = e1 inh {acc = id}
                                                              (e2', trans2) = e2 inh {acc = id}
                                                              [var1, var2] = getVars 2
                                                          in (BinOp (ExpName (Name [var1])) op (ExpName (Name [var2])), trans1 . substVar' inh var1 e1' . trans2 . substVar' inh var2 e2' . acc inh) -- Side effects of the first expression are applied before the second is evaluated
    fInstanceOf                                         = error "instanceOf"
    fCond g e1 e2 inh                                   = let (e1', trans1) = e1 inh {acc = id}
                                                              (e2', trans2) = e2 inh {acc = id}
                                                              (g', transg)  = g inh {acc = id}
                                                          in (Cond g' e1' e2', (transg . (\q -> (g' &* trans1 q) |* (neg g' &* trans2 q)) . acc inh))
    fAssign lhs op e inh                                = let (lhs', lhsTrans) = foldLhs inh {acc = id} lhs
                                                              rhs' = desugarAssign lhs' op e'
                                                              (e', trans) = e inh {acc = id}
                                                          in  (rhs', lhsTrans . trans . substVar (env inh) (decls inh) lhs' rhs' . acc inh)
    fLambda                                             = error "lambda"
    fMethodRef                                          = error "method reference"
                            
    -- gets the transformer for array access (as array access may throw an error)
    arrayAccessWlp :: Exp -> [Exp] -> Inh -> Exp -> Exp
    arrayAccessWlp a i inh q =  case catch inh of
                                    Nothing      -> Cond (foldr (\(i, l) e -> e &* (BinOp i LThan l) &* (BinOp i GThanE (Lit (Int 0)))) true (zip i (dimLengths a))) q false
                                    Just (cs, f) -> let e = InstanceCreation [] (ClassType [(Ident "ArrayIndexOutOfBoundsException", [])]) [] Nothing
                                                    in Cond (foldr (\(i, l) e -> e &* (BinOp i LThan l) &* (BinOp i GThanE (Lit (Int 0)))) true (zip i (dimLengths a))) q ((maybe (if f then q else q &* throwException e)) (\b -> wlp' (inh {acc = id, catch = Nothing}) (StmtBlock b) q) (getCatch (decls inh) (env inh) e cs))
                                
    dimLengths a = case a of
                    ArrayCreate t exps dim          -> exps
                    _                               -> map (\n -> MethodInv (MethodCall (Name [Ident "*length"]) [a, (Lit (Int n))])) [0..]
                    
    -- Edits a name expression to handle build-in constructs
    editName :: Inh -> Name -> Exp
    editName inh (Name name) | last name == Ident "length" = case lookupType (decls inh) (env inh) (Name (take (length name - 1) name)) of -- For arrays we know that "length" refers to the length of the array
                                                                RefType (ArrayType _) -> MethodInv (MethodCall (Name [Ident "*length"]) [ExpName (Name (take (length name - 1) name)), (Lit (Int 0))])
                                                                _ -> ExpName (Name name)
                             | otherwise = ExpName (Name name)
                    
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
    
    -- Folds the expression part of an lhs
    foldLhs :: Inh -> Lhs -> (Lhs, Syn)
    foldLhs inh lhs  = case lhs of
                            FieldLhs (PrimaryFieldAccess e id)  -> case foldExp wlpExpAlgebra e inh of
                                                                    (ExpName name, trans)   -> (NameLhs (Name (fromName name ++ [id])), trans)
                                                                    _                       -> error "foldFieldAccess"
                            ArrayLhs (ArrayIndex a i)           ->  let (a', aTrans) = foldExp wlpExpAlgebra a inh
                                                                        i' = map (\x -> foldExp wlpExpAlgebra x inh) i
                                                                    in (ArrayLhs (ArrayIndex a' (map fst i')), foldl (\trans (_, iTrans) -> trans . iTrans) aTrans i' . arrayAccessWlp a' (map fst i') inh)
                            lhs'                                -> (lhs', id)
    
    -- Folds the expression part of a fieldaccess and simplifies it to a name
    foldFieldAccess :: Inh -> FieldAccess -> Name
    foldFieldAccess inh fieldAccess  = case fieldAccess of
                                            PrimaryFieldAccess e id     -> case fst (foldExp wlpExpAlgebra e inh) of
                                                                                ExpName name    -> Name (fromName name ++ [id])
                                                                                x               -> error ("foldFieldAccess: " ++ show x ++ show id)
                                            SuperFieldAccess id         -> foldFieldAccess inh (PrimaryFieldAccess (fromJust' "foldFieldAccess" (object inh)) id)
                                            ClassFieldAccess name id    -> Name (fromName name ++ [id])
    
    
-- Simplified version of substVar, handy to use with introduced variables
substVar' :: Inh -> Ident -> Exp -> Syn
substVar' inh var e = substVar (env inh) (decls inh) (NameLhs (Name [var])) e

-- | Calculates the weakest liberal pre-condition of a statement and a given post-condition
wlp :: [TypeDecl] -> Stmt -> Exp -> Exp
wlp decls = wlpWithEnv decls []

-- | wlp with a given type environment
wlpWithEnv :: [TypeDecl] -> TypeEnv -> Stmt -> Exp -> Exp
wlpWithEnv decls env = wlp' (Inh id id Nothing env decls [] Nothing Nothing)

-- wlp' lets you specify the inherited attributes
wlp' :: Inh -> Stmt -> Syn
wlp' inh s = foldStmt wlpStmtAlgebra s inh


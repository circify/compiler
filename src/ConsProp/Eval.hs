-- All Helper functions are not intended to be used as standalone functions
-- The main problem here is that I don't want the Abstract Monad to be wrapped
-- around the actual translation unit
-- The result is that only half of the operations are under Monad Operation,
-- while the other half operate directly above the Abstract Monad
-- The main division is during evalStmt:
-- all stmts that has sub-stmts can not be operated inside the Abstract Monad

module ConsProp.Eval where

import ConsProp.Init
import ConsProp.Operation
import ConsProp.Apron.Abstract1
import ConsProp.Apron.Texpr1
import ConsProp.Apron.AbstractMonad
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Data.Char (ord)
import Data.List (last, init)

-----

{- Program -}
evalProg :: CTranslationUnit AbsState -> Abstract (CTranslationUnit AbsState)
evalProg (CTranslUnit extDecls (State a loc)) = do
  labs <- a
  abs <- abstractTop
  nExtDecls <- evalEDLst abs extDecls
  let nSt = State (return abs) loc
  return (CTranslUnit nExtDecls nSt)

evalEDLst :: Abstract1 -> [CExternalDeclaration AbsState] -> Abstract [CExternalDeclaration AbsState]
evalEDLst _ []       = return []
evalEDLst a (ed:eds) = do
  (newAbs, newED) <- evalExtDecl a ed
  nextEDs <- evalEDLst newAbs eds
  return ([newED] ++ nextEDs)

evalExtDecl :: Abstract1 -> CExternalDeclaration AbsState -> Abstract (Abstract1, CExternalDeclaration AbsState)
evalExtDecl abs (CDeclExt decl) = do
  (nAbs, nDecl) <- evalDecl abs "" decl
  return (nAbs, CDeclExt nDecl)
evalExtDecl abs de@(CFDefExt func@(CFunDef _ declr _ _ _))
  | isMainFunc declr = do
    (nAbs, nFunc) <- evalFunc abs func
    return (nAbs, CFDefExt nFunc)
  -- If not the main function, simply skip it
  | otherwise = return (abs, de)
evalExtDecl _ _            = error "CAsmExt not implemented"

-- Helper Function to determine if the function is the main function
isMainFunc :: CDeclarator a -> Bool
isMainFunc (CDeclr (Just (Ident f _ _)) _ _ _ _) =
  case f of
    "main" -> True
    _      -> False
isMainFunc _ = False

-----

{- Functions -}
-- ignore arguments for now
evalFunc :: Abstract1 -> CFunctionDef AbsState -> Abstract (Abstract1, CFunctionDef AbsState)
evalFunc abs (CFunDef a b@(CDeclr (Just (Ident f _ _)) _ _ _ _) c stmt st) = do
  (nAbs, nStmt) <- evalStmt abs f stmt
  nSt <- setAbs nAbs st
  return (nAbs, CFunDef a b c nStmt nSt)
evalFunc _ _ = error "Function Case not implemented"

-----

{- Declarations -}
absAssgHelper :: Abstract Abstract1 -> (VarName, Texpr1) -> Abstract Abstract1
absAssgHelper a (v, t) = do
  a1 <- a
  a2 <- abstractTop
  abstractAssignTexprArray a1 [v] t 1 a2

-- Right now that we don't care about types, the only concern is
-- with the initializers and expressions
-- Variable f for the name of the function we're in so we can determine
-- the scope. f = "" indicates that this is a global environment
evalDecl :: Abstract1 -> String -> CDeclaration AbsState -> Abstract (Abstract1, CDeclaration AbsState)
evalDecl abs f (CDecl a b st) = do
  nAbs <- foldl (\a b -> evalDeclHelper a f b) (return abs) b
  nSt <- setAbs nAbs st
  return (nAbs, CDecl a b nSt)
evalDecl _ _ _  = error "Declaration case not implemented"

-- Process every variable declaration separately
evalDeclHelper :: Abstract Abstract1 -> String -> (Maybe (CDeclarator AbsState), Maybe (CInitializer AbsState), Maybe (CExpression AbsState)) -> Abstract Abstract1
-- If it is only a declaration, no need to change the abstraction
evalDeclHelper a _ (_, Nothing, Nothing) = a

-- A special case for String Assignment
-- All the variable assignments are taken case in the (var, texpr) pair
-- The standalone texpr returned should be empty
evalDeclHelper a f (Just (CDeclr (Just id) _ _ _ _), (Just (CInitExpr expr@(CConst (CStrConst _ _)) st)), Nothing) = do
  abs1 <- a
  let assgExpr = CAssign CAssignOp (CVar id st) expr st
  (_, pair) <- evalExpr abs1 f assgExpr
  foldl absAssgHelper a pair

evalDeclHelper a f (Just (CDeclr (Just (Ident v _ _)) _ _ _ _), (Just (CInitExpr expr _)), Nothing) = do
  abs1 <- a
  (texpr, pair) <- evalExpr abs1 f expr
  var <- findScope v f
  let npair = pair ++ [(var, texpr)]
  foldl absAssgHelper a npair

evalDeclHelper _ _ _ = error "Declaration type not implemented"

-----

{- Statements -}
-- helper function
setAbs :: Abstract1 -> AbsState -> Abstract AbsState
setAbs newAbs (State a loc) = do
  oldAbs <- a
  abs <- abstractJoin oldAbs newAbs
  return (State (return abs) loc)

-- A helper function to evaluate compounds
evalCBI :: Abstract1 -> String -> CCompoundBlockItem AbsState -> Abstract (Abstract1, CCompoundBlockItem AbsState)
evalCBI abs f (CBlockStmt stmt) = do
  (nAbs, nStmt) <- evalStmt abs f stmt
  return (nAbs, CBlockStmt nStmt)
evalCBI abs f (CBlockDecl decl) = do
  (nAbs, nDecl) <- evalDecl abs f decl
  return (nAbs, CBlockDecl nDecl)
evalCBI _ _ _ = error "CBI nested function type not implemented"

-- Evaluating Compound Block Item List
evalCBIs :: Abstract1 -> String -> [CCompoundBlockItem AbsState] -> Abstract (Abstract1, [CCompoundBlockItem AbsState])
evalCBIs abs _ [] = return (abs, [])
evalCBIs abs f (cbi:cbis) = do
  (nextAbs, nCbi)   <- evalCBI abs f cbi
  (finalAbs, fCbis) <- evalCBIs nextAbs f cbis
  return (finalAbs, [nCbi] ++ fCbis)

-- The integer is the iteration bound
-- i.e. how many more iterations until we do the widening
evalLoop :: Abstract1 -> String -> CStatement AbsState -> Integer -> Abstract (Abstract1, CStatement AbsState)

-- While Loop
evalLoop lastAbs f whileStmt@(CWhile cond stmt dw st) n = do
  itAbs          <- evalCons lastAbs f cond False
  (ntAbs, nStmt) <- evalStmt itAbs f stmt
  nAbs           <- case (n < 0) of
                      True  -> abstractWiden lastAbs ntAbs
                      False -> abstractJoin lastAbs ntAbs
  leqEval        <- abstractIsLeq nAbs lastAbs
  nSt <- setAbs lastAbs st
  case leqEval of
    True  -> return (lastAbs, CWhile cond nStmt dw nSt)
    False -> evalLoop nAbs f whileStmt (n - 1)

-- For Loop
-- Convert: for (init; bound; step) do content
-- to:      init; while(bound) do (content; step)
-- Init is dealt in evalStmt
evalLoop lastAbs f forStmt@(CFor init bound step stmt st) n = do
  itAbs <- case bound of
    Nothing   -> return lastAbs
    Just cond -> evalCons lastAbs f cond False
  (ltAbs, nStmt) <- evalStmt itAbs f stmt
  dummyTexpr <- texprMakeConstant 0
  (_, pair) <- case step of
    Nothing   -> return (dummyTexpr, [])
    Just expr -> evalExpr ltAbs f expr
  ntAbs    <- foldl absAssgHelper (return ltAbs) pair
  nAbs     <- case (n <= 0) of
                True  -> abstractWiden lastAbs ntAbs
                False -> abstractJoin lastAbs ntAbs
  leqEval  <- abstractIsLeq nAbs lastAbs
  nSt <- setAbs lastAbs st
  case leqEval of
    True  -> return (lastAbs, CFor init bound step nStmt nSt)
    False -> evalLoop nAbs f forStmt (n - 1)

evalLoop _ _ _ _ = error "Loop Statement not implemented"

-- Narrowing a loop
-- The idea is that after computing a fixpoint of the loop, the result of
-- executing one more iteration based on that fixpoint must still be sound.
-- Furthermore, this result can only be less or equal to the previous one.
-- Thus by iterating one more loop, we might be able to obtain a more precise
-- abstraction.
evalNarrow :: Abstract1 -> String -> CExpression AbsState -> CStatement AbsState -> Maybe (CExpression AbsState) -> Abstract Abstract1
evalNarrow lastAbs f cond stmt step = do
  iAbs      <- evalCons lastAbs f cond False
  (lAbs, _) <- evalStmt iAbs f stmt
  dummyTexpr <- texprMakeConstant 0
  (_, pair)  <- case step of
    Nothing   -> return (dummyTexpr, [])
    Just expr -> evalExpr lAbs f expr
  rAbs    <- foldl absAssgHelper (return lAbs) pair
  nAbs    <- abstractMeet lastAbs rAbs
  return nAbs

evalStmt :: Abstract1 -> String -> CStatement AbsState -> Abstract (Abstract1, CStatement AbsState)

-- Expression Statements
evalStmt a _ (CExpr Nothing st) = do
  nSt <- setAbs a st
  return (a, CExpr Nothing nSt)
-- We can disregard the side effect of the expression
evalStmt a f (CExpr (Just expr) st) = do
  (_, pair) <- evalExpr a f expr
  nAbs <- foldl absAssgHelper (return a) pair
  nSt <- setAbs nAbs st
  return (nAbs, CExpr (Just expr) nSt)

-- Return Statements
-- Assume for now that nothing will be behind the return statement
evalStmt a _ (CReturn Nothing st) = do
  nSt <- setAbs a st
  return (a, CReturn Nothing nSt)
evalStmt a f (CReturn (Just expr) st) = do
  (texpr, pair) <- evalExpr a f expr
  let npair = pair ++ [(f, texpr)]
  nAbs <- foldl absAssgHelper (return a) npair
  nSt <- setAbs nAbs st
  return (nAbs, CReturn (Just expr) nSt)

-- Compound Statments
-- Since we will reach this statement after every if-else and loop,
-- it is a good idea to check if the abstract1 is Bottom
-- If it is, there is no need to continue
evalStmt a f (CCompound ids cbis st) = do
  isBot <- abstractIsBottom a
  (nAbs, ncbis) <- case isBot of
                     True  -> return (a, cbis)
                     False -> evalCBIs a f cbis
  nSt <- setAbs nAbs st
  return (nAbs, CCompound ids ncbis nSt)

-- If Statements
evalStmt a f (CIf cons tstmt Nothing st) = do
  itAbs           <- evalCons a f cons False
  (ntAbs, ntStmt) <- evalStmt itAbs f tstmt
  nfAbs           <- evalCons a f cons True
  nAbs            <- abstractJoin ntAbs nfAbs
  nSt <- setAbs nAbs st
  return (nAbs, CIf cons ntStmt Nothing nSt)
evalStmt a f (CIf cons tstmt (Just fstmt) st) = do
  itAbs           <- evalCons a f cons False
  ifAbs           <- evalCons a f cons True
  (ntAbs, ntStmt) <- evalStmt itAbs f tstmt
  (nfAbs, nfStmt) <- evalStmt ifAbs f fstmt
  nAbs            <- abstractJoin ntAbs nfAbs
  nSt <- setAbs nAbs st
  return (nAbs, CIf cons ntStmt (Just nfStmt) nSt)

-- Loops
-- Loop analysis is separated into 2 parts:
--   Case 1: the loop will be executed is handled in EvalLoop and
--   subsequently EvalNarrow
--   Case 2: the loop will not be executed is handled in evalStmt
evalStmt abs f whileStmt@(CWhile cond stmt dw _) = do
  (a, _) <- case dw of
    True  -> evalStmt abs f stmt
    False -> return (abs, stmt)
  (lnAbs, (CWhile _ nStmt _ nSt)) <- evalLoop a f whileStmt 0
  lfAbs <- evalNarrow lnAbs f cond stmt Nothing
  lAbs <- evalCons lfAbs f cond True
  -- Now deal with the case where the loop will not be executed
  rAbs <- evalCons abs f cond True
  finalAbs <- abstractJoin lAbs rAbs
  fSt <- setAbs finalAbs nSt
  return (finalAbs, (CWhile cond nStmt dw fSt))

-- We want to deal with init in the for loop
-- If there is no condition, no need to narrow it
-- The loop must execute, so we don't need to worry about Case 2 either
evalStmt abs f forStmt@(CFor init Nothing _ _ _) = do
  a <- evalInit abs f init
  evalLoop a f forStmt 0
evalStmt abs f forStmt@(CFor init (Just cond) step stmt _) = do
  a <- evalInit abs f init
  (lnAbs, (CFor _ _ _ nStmt nSt)) <- evalLoop a f forStmt 0
  lfAbs <- evalNarrow lnAbs f cond stmt step
  lAbs <- evalCons lfAbs f cond True
  -- Now deal with the case where the loop will not be executed
  -- Technically this shouldn't happen
  rAbs <- evalCons a f cond True
  finalAbs <- abstractJoin lAbs rAbs
  fSt <- setAbs finalAbs nSt
  return (finalAbs, (CFor init (Just cond) step nStmt fSt))

-- Others
evalStmt _ _ stmt = error ("Statement Case not implemented: " ++ (show stmt))

-- Just a helper function for For Loop
evalInit :: Abstract1 -> String -> (Either (Maybe (CExpression AbsState)) (CDeclaration AbsState)) -> Abstract Abstract1
evalInit a _ (Left Nothing) = return a
evalInit a f (Left (Just expr)) = do
  (_, pair) <- evalExpr a f expr
  iAbs <- foldl absAssgHelper (return a) pair
  return iAbs
evalInit a f (Right decl) = do
  (iAbs, _) <- evalDecl a f decl
  return iAbs

-----

{- Expressions -}
-- A special return type for evaluating an expression
-- An expression can generate a syntax tree, but might also involve assignments
type ExprSt = (Texpr1, [(VarName, Texpr1)])

-- f is the name of Function we're currently in
-- Convention: only check the scope when directly referencing the variable
-- Either in assignment or variable expression
-- Every constraint evaluated in evalExpr is treated as a number
-- i.e. evaluate the constraint and return 0 or 1 based on result
evalExpr :: Abstract1 -> String -> CExpression AbsState -> Abstract ExprSt

-- String Assignment: int a[10] = "hello"
-- We need to convert "hello" into "hello\O" first
evalExpr _ f (CAssign CAssignOp (CVar (Ident v _ _) _) (CConst (CStrConst (CString s _) _)) _) = do
  vLst <- evalList (map ord (s ++ "\0"))
  var <- findScope v f
  let assgLst = evalStringAssg var ((length vLst) - 1) vLst
  dummyTexpr <- texprMakeConstant 1
  return (dummyTexpr, assgLst)

evalExpr a f (CAssign CAssignOp expr rhs _) = do
  -- Technically ++a would be different but ignore it for now
  var <- case expr of
           CVar (Ident v _ _) _ -> findScope v f
           CIndex _ _ _         -> evalIndex a f expr
           _ -> error "Unsupported Assignment Operation"
  (rtexpr, rpair) <- evalExpr a f rhs
  return (rtexpr, rpair ++ [(var, rtexpr)])

evalExpr a f (CAssign aop (CVar (Ident v _ _) _) rhs _) = do
  var    <- findScope v f
  ltexpr <- texprMakeLeafVar var
  (rtexpr, rpair) <- evalExpr a f rhs
  ntexpr <- evalBOpExpr (convertAOp aop) ltexpr rtexpr
  return (ntexpr, rpair ++ [(var, ntexpr)])

evalExpr _ f (CVar (Ident v _ _) _) = do
  var    <- findScope v f
  ntexpr <- texprMakeLeafVar var
  return (ntexpr, [])

evalExpr _ _ (CConst (CIntConst n _)) = do
  ntexpr <- texprMakeConstant (fromInteger (getCInteger n))
  return (ntexpr, [])

evalExpr a f (CBinary bop expr1 expr2 _)
  | isBOpCons bop = error "Boolean evaluation not supported"
  | isBOpLogic bop = error "Boolean evaluation not supported"
  | otherwise = do
    (ltexpr, lpair) <- evalExpr a f expr1
    (rtexpr, rpair) <- evalExpr a f expr2
    ntexpr <- evalBOpExpr bop ltexpr rtexpr
    return (ntexpr, lpair ++ rpair)

evalExpr a f (CUnary uop expr _) = do
  st@(rtexpr, rpair) <- evalExpr a f expr
  ntexpr <- evalUOpExpr uop rtexpr
  case uop of
    CPreIncOp  -> incDecHelper expr f uop ntexpr st
    CPreDecOp  -> incDecHelper expr f uop ntexpr st
    CPostIncOp -> incDecHelper expr f uop ntexpr st
    CPostDecOp -> incDecHelper expr f uop ntexpr st
    -- The Plus Operator literally does nothing
    _          -> return (ntexpr, rpair)

evalExpr a f e@(CIndex _ _ _) = do
  var    <- evalIndex a f e
  ntexpr <- texprMakeLeafVar var
  return (ntexpr, [])

evalExpr _ _ e = error ("expression not implemented: " ++ (show e))

evalBool :: Bool -> Integer
evalBool b =
  case b of
    True  -> 1
    False -> 0

-- Convert a list of Int to a list of Texpr1
evalList :: [Int] -> Abstract [Texpr1]
evalList []     = return []
evalList (i:is) = do
  t  <- texprMakeConstant i
  ts <- evalList is
  return ([t] ++ ts)

-- Obtain the correct variable when given an array
-- Scope is applied in evalExpr
-- Assume that every index is an integer (not variable or expression)
evalIndex :: Abstract1 -> String -> CExpression AbsState -> Abstract String
evalIndex a f (CIndex l r _) = do
  lst <- case l of
           CIndex _ _ _ -> evalIndex a f l
           CVar (Ident v _ _) _ -> findScope v f
           _ -> error "Unsupported Array Index"
  (rtexpr, _) <- evalExpr a f r
  n <- abstractTexprEval a rtexpr
  let rst = "#" ++ (show n)
  return (lst ++ rst)
evalIndex _ _ _ = error "Invalid Index Operation"

evalStringAssg :: String -> Int -> [Texpr1] -> [(VarName, Texpr1)]
evalStringAssg v 0 [t] = [(v ++ "#0", t)]
evalStringAssg v i ts =
  (evalStringAssg v (i - 1) nts) ++ [(v ++ "#" ++ (show i), t)]
  where t   = last ts
        nts = init ts

-- A helper function to deal with ++ and --
incDecHelper :: CExpression AbsState -> String -> CUnaryOp -> Texpr1 -> ExprSt -> Abstract ExprSt
incDecHelper (CVar (Ident v _ _) _) f uop ntexpr (rtexpr, rpair) = do
  -- expr must be in the form of CVar
  var <- findScope v f
  let npair = rpair ++ [(var, ntexpr)]
  case uop of
    CPreIncOp  -> return (ntexpr, npair) -- ++a
    CPreDecOp  -> return (ntexpr, npair) -- --a
    CPostIncOp -> return (rtexpr, npair) -- a++
    CPostDecOp -> return (rtexpr, npair) -- a--
    _          -> error "Unsupported UOP Operation"
incDecHelper _ _ _ _ _ = error "Invalid IncDec Operation"

-- Evaluate Constraints, evaluate side effects, meet constraint with abstract1
-- Bool is to determine if we want !(constraint)
-- True if we want !(constraint)
-- evalCons takes in Abstract Abstract1, as oppose to evalExpr taking Abstract1
evalCons :: Abstract1 -> String -> CExpression AbsState -> Bool -> Abstract Abstract1
evalCons a f e@(CBinary bop expr1 expr2 _) neg
  | isBOpCons bop = do
    (ltexpr, lpair) <- evalExpr a f expr1
    (rtexpr, rpair) <- evalExpr a f expr2
    lAbs   <- foldl absAssgHelper (return a) (lpair ++ rpair)
    ntcons <- evalBOpCons bop ltexpr rtexpr neg
    nAbs   <- abstractTconsMeet lAbs ntcons
    return nAbs
  | (isBOpLogic bop) && (not neg) = do
    lAbs <- evalCons a f expr1 False
    rAbs <- evalCons a f expr2 False
    case bop of
      CLndOp -> abstractMeet lAbs rAbs
      CLorOp -> abstractJoin lAbs rAbs
      _      -> error "Unsupported BOP Operation"
  | isBOpLogic bop = do
    lAbs <- evalCons a f expr1 True
    rAbs <- evalCons a f expr2 True
    case bop of
      CLndOp -> abstractJoin lAbs rAbs
      CLorOp -> abstractMeet lAbs rAbs
      _      -> error "Unsupported BOP Operation"
  | otherwise     = error ("Int to Bool Conversion not supported: " ++ show e)
evalCons a f e@(CUnary uop expr _) neg
  | uop == CNegOp = evalCons a f expr (not neg)
  | otherwise     = error ("Int to Bool Conversion not supported: " ++ show e)
evalCons _ _ e _  = error ("Int to Bool Conversion not supported: " ++ show e)

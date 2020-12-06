-- A document of all helper functions that convert complex
-- operations in C to Texpr and Tcons in APRON

module ConsProp.Operation where
import ConsProp.Apron.AbstractMonad
import ConsProp.Apron.Texpr1
import ConsProp.Apron.Tcons1
import ConsProp.Apron.Lincons1
import ConsProp.Apron.TexprOp
import Language.C.Syntax.AST
import Control.Monad.State.Strict (liftIO)


{- Binary Operations -}

evalBOpExpr :: CBinaryOp -> Texpr1 -> Texpr1 -> Abstract Texpr1
evalBOpExpr bop l r
  | simpleBOp bop = do
    n <- texprMakeBinOp (evalSimpleBOp bop) l r ROUND_INT ROUND_DOWN
    return n
  | otherwise = error "Not Implemented"

-- Bool is to determine if we want !(constraint)
-- True if we want !(constraint)
evalBOpCons :: CBinaryOp -> Texpr1 -> Texpr1 -> Bool -> Abstract Tcons1
evalBOpCons bop t1 t2 neg
  -- For form a < b, we need to convert to (b - a) > 0
  | (nbop == CLeOp) || (nbop == CLeqOp) = do
    l <- texprMakeBinOp SUB_OP t2 t1 ROUND_INT ROUND_DOWN
    let r = 0
    n <- tconsMake (evalConsBOp nbop) l r
    return n
  -- If the operation is not a constraint, evalConsBOp would throw an error
  | otherwise = do
    l <- texprMakeBinOp SUB_OP t1 t2 ROUND_INT ROUND_DOWN
    let r = 0
    n <- tconsMake (evalConsBOp nbop) l r
    return n
  where nbop = negConsBOp bop neg

{- Simple BOp Case -}
-- True if bop has an APRON correspondence
simpleBOp :: CBinaryOp -> Bool
simpleBOp bop =
  case bop of
    CMulOp -> True
    CDivOp -> True
    CRmdOp -> True
    CAddOp -> True
    CSubOp -> True
    _      -> False

--  Convert simple BOp to their APRON correspondence
evalSimpleBOp :: CBinaryOp -> OpType
evalSimpleBOp bop =
  case bop of
    CMulOp -> MUL_OP
    CDivOp -> DIV_OP
    CRmdOp -> MOD_OP
    CAddOp -> ADD_OP
    CSubOp -> SUB_OP
    _      -> error ("Not a simple BOp" ++ show bop)

evalConsBOp :: CBinaryOp -> Constyp
evalConsBOp bop =
  case bop of
    CLeOp  -> CONS_SUP
    CGrOp  -> CONS_SUP
    CLeqOp -> CONS_SUPEQ
    CGeqOp -> CONS_SUPEQ
    CEqOp  -> CONS_EQ
    CNeqOp -> CONS_DISEQ
    _      -> error ("Not a constraint BOp" ++ show bop)

-- If neg is true, revert the Constraint Operation
negConsBOp :: CBinaryOp -> Bool -> CBinaryOp
negConsBOp bop neg
  | neg =
    case bop of
      CLeOp  -> CGeqOp
      CGrOp  -> CLeqOp
      CLeqOp -> CGrOp
      CGeqOp -> CLeOp
      CEqOp  -> CNeqOp
      CNeqOp -> CEqOp
      _      -> error "Not a constraint"
  | otherwise = bop

{- Assignment Operations -}

-- Convert AOp to BOp for evaluation
convertAOp :: CAssignOp -> CBinaryOp
convertAOp aop =
  case aop of
    CMulAssOp -> CMulOp
    CDivAssOp -> CDivOp
    CRmdAssOp -> CRmdOp
    CAddAssOp -> CAddOp
    CSubAssOp -> CSubOp
    CShlAssOp -> CShlOp
    CShrAssOp -> CShrOp
    CAndAssOp -> CAndOp
    CXorAssOp -> CXorOp
    COrAssOp  -> COrOp


{- Unary Operations -}

evalUOpExpr :: CUnaryOp -> Texpr1 -> Abstract Texpr1
evalUOpExpr uop r
  | isIncDec uop   = evalIncDec uop r
  | uop == CPlusOp = return r
  | uop == CMinOp  = do
    l <- texprMakeConstant (-1)
    n <- texprMakeBinOp MUL_OP l r ROUND_INT ROUND_DOWN
    return n
  | otherwise      = error "Not Implemented"

evalUOpCons :: CUnaryOp -> Texpr1 -> Abstract Tcons1
evalUOpCons uop t = error "Not Implemented"

{- ++ and -- Case -}
isIncDec :: CUnaryOp -> Bool
isIncDec uop =
  case uop of
    CPreIncOp  -> True
    CPreDecOp  -> True
    CPostIncOp -> True
    CPostDecOp -> True
    _          -> False

evalUOp :: CUnaryOp -> OpType
evalUOp uop =
  case uop of
    CPreIncOp  -> ADD_OP
    CPreDecOp  -> SUB_OP
    CPostIncOp -> ADD_OP
    CPostDecOp -> SUB_OP
    _          -> error "Unsupported UOp"

evalIncDec :: CUnaryOp -> Texpr1 -> Abstract Texpr1
evalIncDec uop l = do
  r <- texprMakeConstant 1
  n <- texprMakeBinOp (evalUOp uop) l r ROUND_INT ROUND_DOWN
  return n

-- Helper Functions
-- Helper Function to determine if we are evaluating a tree expression
-- or a tree constraint
-- True if the operation is a constraint operation
isBOpCons :: CBinaryOp -> Bool
isBOpCons bop =
  case bop of
    CLeOp  -> True
    CGrOp  -> True
    CLeqOp -> True
    CGeqOp -> True
    CEqOp  -> True
    CNeqOp -> True
    _      -> False

isBOpLogic :: CBinaryOp -> Bool
isBOpLogic bop =
  case bop of
    CLndOp -> True
    CLorOp -> True
    _      -> False


module ConsProp.Apron.Abstract1  where
import           ConsProp.Apron.AbstractMonad
import           ConsProp.Apron.Var
import           ConsProp.Apron.TexprOp
import           ConsProp.Apron.Tcons1
import           ConsProp.Apron.Texpr1
import           Control.Monad.State.Strict
import qualified Data.Map as M

type VarMap = M.Map VarName Var

-- Bool is used to determine if the state is unreachable
-- If it is unreachable, the values no longer matter
-- Once an abstract state becomes unreachable, there is really no way to
-- make it reachable again, except reinitialize and join
data Abstract1 = Abs1 VarMap Bool

-- Constructors

abstractBottom :: Abstract Abstract1
abstractBottom = do
  -- No need to initialize the mapping since Bottom is a placeholder anyways
  let abs = M.empty
  return (Abs1 abs True)

abstractTop :: Abstract Abstract1
abstractTop = do
  (Env vs) <- gets unEnvironment
  let abs = M.fromList (map (\a -> (a, Top)) vs)
  return (Abs1 abs False)

-- Printing

abstractPrint :: Abstract1 -> Abstract ()
abstractPrint a = do
  liftIO $ printAbstract1 a

printAbstract1 :: Abstract1 -> IO ()
printAbstract1 (Abs1 _ True) = do
  putStrLn "Variables: "
  putStrLn "  Unreachable"
printAbstract1 (Abs1 vm _) = do
  putStrLn "Variables: "
  -- putStrLn "  Reachable"
  printAbsLst (M.toAscList vm)

printAbsLst :: [(String, Var)] -> IO ()
printAbsLst [] = return ()
printAbsLst ((s, v):ls) = do
  case v of
    Top     -> putStrLn ("  " ++ s ++ ": Top")
    Bottom  -> putStrLn ("  " ++ s ++ ": Bottom")
    Const c -> putStrLn ("  " ++ s ++ ": " ++ (show c))
  printAbsLst ls

-- Tests

abstractIsBottom :: Abstract1 -> Abstract Bool
abstractIsBottom (Abs1 _ bot) = do
  return bot

-- Every Variable is Top
abstractIsTop :: Abstract1 -> Abstract Bool
abstractIsTop (Abs1 vs bot) = do
  let it = foldl (&&) True (M.map (\a -> (isTop a)) vs)
  case (it, bot) of
    (_    , True)  -> return False
    (True , False) -> return True
    (False, False) -> return False

-- We have to assume that the variables are the same. Otherwise abstractIsLeq
-- won't make sense
abstractIsLeq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsLeq (Abs1 _ True) _ = return True
abstractIsLeq _ (Abs1 _ True) = return False
abstractIsLeq (Abs1 vs1 _) (Abs1 vs2 _) = do
  let nvs = M.intersectionWith isLeq vs1 vs2
  return (M.foldl (&&) True nvs)

-- Again, assume that the variables are the same.
abstractIsEq :: Abstract1 -> Abstract1 -> Abstract Bool
abstractIsEq (Abs1 _ True) (Abs1 _ True) = return True
abstractIsEq (Abs1 vs1 False) (Abs1 vs2 False) = do
  let nvs = M.intersectionWith isEq vs1 vs2
  return (M.foldl (&&) True nvs)
abstractIsEq _ _ = return False

-- Operations

-- | Meet of two abstract values.
abstractMeet :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractMeet (Abs1 _ True) _           = return (Abs1 M.empty True)
abstractMeet _ (Abs1 _ True)           = return (Abs1 M.empty True)
abstractMeet (Abs1 vs1 _) (Abs1 vs2 _) = do
  let nvs = M.unionWith absMeet vs1 vs2
  return (Abs1 nvs False)

-- | Join of two abstract values.
abstractJoin :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractJoin (Abs1 _ True) a           = return a
abstractJoin a (Abs1 _ True)           = return a
abstractJoin (Abs1 vs1 _) (Abs1 vs2 _) = do
  let nvs = M.unionWith absJoin vs1 vs2
  return (Abs1 nvs False)

abstractTconsMeet :: Abstract1 -> Tcons1 -> Abstract Abstract1
-- TODO: We have to deal the case of EQ separately
-- Because in EQ, we might obtain some additional information of the variable
-- e.g. from a + 3 = 5 we can obtain that a = 2
-- abstractTconsMeet a (Tcons1 CONS_EQ texpr s) = do
abstractTconsMeet a (Tcons1 op texpr s) = do
  v <- abstractTexprSolve a texpr
  case (constypEval op v s) of
    True  -> return a
    False -> return (Abs1 M.empty True)

abstractWiden :: Abstract1 -> Abstract1 -> Abstract Abstract1
abstractWiden (Abs1 _ True) a           = return a
abstractWiden a (Abs1 _ True)           = return a
abstractWiden (Abs1 vs1 _) (Abs1 vs2 _) = do
  let nvs = M.unionWith widen vs1 vs2
  return (Abs1 nvs False)

-- | Assign a list of variables in the abstract domain to the evaluation
-- a tree expression
abstractAssignTexprArray :: Abstract1 -> [VarName] -> Texpr1 -> Int -> Abstract1 -> Abstract Abstract1
abstractAssignTexprArray (Abs1 _ True) _ _ _ _ = do
  return (Abs1 M.empty True)
abstractAssignTexprArray a@(Abs1 vm _) (var:_) texpr size _ = do
  v <- abstractTexprSolve a texpr
  -- The only way for v to be Bottom is that a variable is unreachable,
  -- and the only way a variable is unreachable is for the whole thing to
  -- be unreachable, which is dealt with in the previous case
  let nvm = M.insert var v vm
  case v of
    Bottom -> error "Invalid Texpr"
    _      -> return (Abs1 nvm False)

abstractTexprEval :: Abstract1 -> Texpr1 -> Abstract Int
abstractTexprEval a t = do
  v <- abstractTexprSolve a t
  case v of
    Const c -> return c
    _       -> error ("Array index is not an integer: " ++ show t ++ " " ++ show v)

-- | Helper Function

abstractTexprSolve :: Abstract1 -> Texpr1 -> Abstract Var
abstractTexprSolve (Abs1 _ True) t = return Bottom
abstractTexprSolve (Abs1 vm _) (Var v) = do
  case M.lookup v vm of
    Nothing  -> error ("Variable does not exist: " ++ v ++ " in " ++ (show vm))
    Just var -> return var
abstractTexprSolve _ (Cst c) = do
  return (Const c)
abstractTexprSolve a (UnOp op texpr) = do
  t <- abstractTexprSolve a texpr
  let v = (unOpTransl op) t
  return v
abstractTexprSolve a (BinOp op texpr1 texpr2) = do
  t1 <- abstractTexprSolve a texpr1
  t2 <- abstractTexprSolve a texpr2
  let v = (binOpTransl op) t1 t2
  return v

binOpTransl :: OpType -> (Var -> Var -> Var)
binOpTransl op =
  case op of
    ADD_OP -> absAdd
    SUB_OP -> absSub
    MUL_OP -> absMul
    DIV_OP -> absDiv
    MOD_OP -> absMod
    _      -> error "Unsupported or invalid binary operation"

unOpTransl :: OpType -> (Var -> Var)
unOpTransl op =
  case op of
    NEG_OP -> absNeg
    _      -> error "Unsupported or invalid unary operation"

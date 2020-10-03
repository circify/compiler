{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Codegen.C where
import           AST.C
import           Codegen.C.CUtils
import           Codegen.C.Utils
import           Codegen.Circify
import           Codegen.Circify.Memory         ( MonadMem
                                                , liftMem
                                                , MemState
                                                , Mem
                                                )
import           Control.Monad                  ( join
                                                , replicateM_
                                                , forM
                                                )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Char                      ( ord
                                                , toLower
                                                )
import qualified Data.Char                     as Char
import           Data.Either                    ( isRight )
import           Data.List                      ( intercalate
                                                , isPrefixOf
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , catMaybes
                                                )
import           IR.SMT.Assert                  ( MonadAssert
                                                , liftAssert
                                                )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import qualified Util.Cfg                      as Cfg
import           Util.Cfg                       ( Cfg
                                                , MonadCfg(..)
                                                )
import           Util.Log


data CState = CState { funs          :: Map.Map FunctionName CFunDef
                     , loopBound     :: Int
                     , findUB        :: Bool
                     , bugConditions :: [Ty.TermBool]
                     , assumptions   :: [Ty.TermBool]
                     , nonDetCtr     :: Int
                     }

newtype C a = C (StateT CState (Circify Type CTerm) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadIO, MonadLog, MonadAssert, MonadMem, MonadCircify Type CTerm, MonadCfg)

emptyCState :: Bool -> CState
emptyCState findBugs = CState { funs          = Map.empty
                              , loopBound     = 5
                              , findUB        = findBugs
                              , bugConditions = []
                              , assumptions   = []
                              , nonDetCtr     = 0
                              }

cfgFromEnv :: C ()
cfgFromEnv = do
  bound <- Cfg.liftCfg $ asks $ Cfg._loopBound
  modify $ \s -> s { loopBound = bound }
  liftLog $ logIf "loop" $ "Setting loop bound to " ++ show bound

-- Loops

getLoopBound :: C Int
getLoopBound = gets loopBound

setLoopBound :: Int -> C ()
setLoopBound bound = modify (\s -> s { loopBound = bound })

-- Functions

registerFunction :: FunctionName -> CFunDef -> C ()
registerFunction name function = do
  s0 <- get
  case Map.lookup name $ funs s0 of
    Nothing -> put $ s0 { funs = Map.insert name function $ funs s0 }
    _       -> error $ unwords ["Already declared", name]

getFunction :: FunctionName -> C CFunDef
getFunction funName = do
  functions <- gets funs
  case Map.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function", funName]

-- Bugs

-- Record that a bug happens if some condition is met (on this path!)
bugIf :: Ty.TermBool -> C ()
bugIf c = do
  g <- liftCircify getGuard
  modify $ \s ->
    s { bugConditions = Ty.BoolNaryExpr Ty.And [g, c] : bugConditions s }

assume :: Ty.TermBool -> C ()
assume c = do
  g <- liftCircify getGuard
  modify
    $ \s -> s { assumptions = Ty.BoolBinExpr Ty.Implies g c : assumptions s }

-- Assert that some recorded bug happens
assertBug :: C ()
assertBug = do
  cs <- gets bugConditions
  liftAssert $ Assert.assert $ Ty.BoolNaryExpr Ty.Or cs
  as <- gets assumptions
  liftAssert $ forM_ as Assert.assert

-- Lift some CUtils stuff to the SSA layer
ssaBool :: CSsaVal -> Ty.TermBool
ssaBool = cBool . ssaValAsTerm "cBool"

ssaType :: CSsaVal -> Type
ssaType = cType . ssaValAsTerm "cType"

ssaLoad :: CSsaVal -> C CSsaVal
ssaLoad addr = case addr of
  Base cterm -> do
    (oob, val) <- liftMem $ cLoad cterm
    whenM (gets findUB) $ bugIf oob
    return $ Base val
  RefVal inner -> liftCircify $ getTerm (SLRef inner)

ssaStore :: CSsaVal -> CSsaVal -> C ()
ssaStore ref val = case (ref, val) of
  (Base addr, Base cval) -> do
    g   <- liftCircify getGuard
    oob <- liftMem $ cStore addr cval g
    whenM (gets findUB) $ bugIf oob
  _ -> error $ "Cannot store " ++ show (ref, val)

ssaStructGet :: String -> CSsaVal -> CSsaVal
ssaStructGet n = liftTermFun "cStructGet" (`cStructGet` n)

ssaStructSet :: String -> CSsaVal -> CSsaVal -> CSsaVal
ssaStructSet n = liftTermFun2 "cStructSet" (cStructSet n)

typedefSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> C (Maybe Type)
typedefSMT (Ident name _ _) tys ptrs = do
  ty <- liftCircify $ ctype tys ptrs
  case ty of
    Right ty' -> liftCircify $ typedef name ty' >> return (Just ty')
    Left  _   -> return Nothing

declareVarSMT
  :: Bool -> Ident -> [CDeclSpec] -> [CDerivedDeclr] -> C (Either String Type)
declareVarSMT isInput (Ident name _ _) tys ptrs = do
  ty <- liftCircify $ ctype tys ptrs
  liftLog $ logIf "decls" $ "declareVarSMT: " ++ show name ++ ": " ++ show ty
  forM_ ty $ liftCircify . declareVar isInput name
  return ty

genVarSMT :: Ident -> C CSsaVal
genVarSMT (Ident name _ _) = liftCircify $ getTerm $ SLVar name

genConstSMT :: CConst -> C CTerm
genConstSMT c = case c of
  CIntConst  (CInteger i _ _) _ -> return $ cIntLit S32 i
  CCharConst (CChar c _     ) _ -> return $ cIntLit S8 $ toInteger $ Char.ord c
  CCharConst (CChars{}      ) _ -> error "Chars const unsupported"
  CFloatConst (Language.C.Syntax.Constants.CFloat str) _ ->
    case toLower (last str) of
      'f' -> return $ cFloatLit (read $ init str)
      'l' -> return $ cDoubleLit (read $ init str)
      _   -> return $ cDoubleLit (read str)
  CStrConst (CString str _) _ -> liftMem
    $ cArrayLit S8 (map (cIntLit S8 . toInteger . ord) str ++ [cIntLit S8 0])

type CSsaVal = SsaVal CTerm

data CLVal = CLVar SsaLVal
           | CLAddr CSsaVal
           | CLField CLVal String
           deriving (Show)

evalLVal :: CLVal -> C CSsaVal
evalLVal location = case location of
  CLVar  v    -> liftCircify $ getTerm v
  CLAddr a    -> ssaLoad a
  CLField s f -> ssaStructGet f <$> evalLVal s

genLValueSMT :: CExpr -> C CLVal
genLValueSMT expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar $ SLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExprSMT addr
  CIndex base   idx  _    -> do
    base' <- genExprSMT base
    idx'  <- genExprSMT idx
    addr  <- liftMem $ liftTermFun2M "cIndex" cIndex base' idx'
    return $ CLAddr addr
  CMember struct ident False _ ->
    flip CLField (identToVarName ident) <$> genLValueSMT struct
  CMember struct ident True _ -> do
    s <- genExprSMT struct
    return $ CLField (CLAddr s) (identToVarName ident)
  _ -> error $ unwords ["Not yet impled:", show expr]

genRefSMT :: CExpr -> C CSsaVal
genRefSMT expr = case expr of
  CVar (Ident name _ _) _ -> liftCircify $ getRef $ SLVar name
  _                       -> error $ unwords ["Not yet impled:", show expr]


-- TODO: This may be broken
-- The approach in `modLocation` is the right one, but when run on addresses it
-- performs accesses to the underlying storage, even when we're doing an
-- overwrite. This may not agree with our uninit tracking system.
genAssign :: CLVal -> CSsaVal -> C CSsaVal
genAssign location value = case location of
  CLVar  varName -> liftCircify $ ssaAssign varName value
  CLAddr addr    -> case addr of
    Base{}   -> ssaStore addr value >> return value
    RefVal r -> liftCircify $ ssaAssign (SLRef r) value
  CLField{} -> modLocation location (const value)
   where
    -- Apply a modification function to a location
    modLocation :: CLVal -> (CSsaVal -> CSsaVal) -> C CSsaVal
    modLocation location modFn = case location of
      CLVar varName ->
        liftCircify $ getTerm varName >>= ssaAssign varName . modFn
      CLAddr addr -> case addr of
        Base _ -> do
          old <- ssaLoad addr
          let new = modFn old
          ssaStore addr new
          return new
        RefVal r ->
          let v = SLRef r in liftCircify $ getTerm v >>= ssaAssign v . modFn
      CLField struct field -> modLocation
        struct
        (\t -> ssaStructSet field (modFn $ ssaStructGet field t) t)

unwrap :: Show l => Either l r -> r
unwrap e = case e of
  Left  l -> error $ "Either is not right, it is: Left " ++ show l
  Right r -> r

noneIfVoid :: Type -> Maybe Type
noneIfVoid t = if Void == t then Nothing else Just t

-- | Handle special functions, returning whether this function was special
genSpecialFunction :: VarName -> [CSsaVal] -> C (Maybe CSsaVal)
genSpecialFunction fnName args = do
  specifialPrintf <- Cfg.liftCfg $ asks (Cfg._printfOutput . Cfg._cCfg)
  svExtensions    <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
  bugs            <- gets findUB
  case fnName of
    "printf" | specifialPrintf -> do
      -- skip fstring
      when bugs $ forM_ (tail args) (bugIf . ssaBool)
      -- Not quite right. Should be # chars.
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_error" | svExtensions -> do
      when bugs $ bugIf (Ty.BoolLit True)
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assert" | svExtensions -> do
      when bugs $ bugIf $ Ty.Not $ ssaBool $ head args
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assume" | svExtensions -> do
      when bugs $ assume $ ssaBool $ head args
      return $ Just $ Base $ cIntLit S32 1
    _ | isNonDet fnName -> do
      let ty = nonDetTy fnName
      n <- gets nonDetCtr
      modify $ \s -> s { nonDetCtr = n + 1 }
      let name = fnName ++ "_" ++ show n
      liftCircify $ declareVar True name ty
      liftCircify $ Just <$> getTerm (SLVar name)
    _ -> return Nothing
 where
  nonDetTy :: String -> Type
  nonDetTy s = case drop (length "__VERIFIER_nondet_") s of
    "int"   -> S32
    "uint"  -> U32
    "long"  -> S64
    "ulong" -> U64
    _       -> error $ "Unknown nondet suffix in: " ++ s
  isNonDet = isPrefixOf "__VERIFIER_nondet_"

genExprSMT :: CExpr -> C CSsaVal
genExprSMT expr = do
  liftLog $ logIfM "expr" $ do
    t <- liftIO $ nodeText expr
    return $ "Expr: " ++ t
  case expr of
    CVar id _            -> genVarSMT id
    CConst c             -> Base <$> genConstSMT c
    CAssign op lhs rhs _ -> do
      lval <- genLValueSMT lhs
      rval <- genExprSMT rhs
      genAssignOp op lval rval
    CBinary op left right _ -> case op of
      CLndOp -> do
        left'  <- genExprSMT left
        right' <- guarded (ssaBool left') $ genExprSMT right
        return $ liftTermFun2 "cAnd" cAnd left' right'
      CLorOp -> do
        left'  <- genExprSMT left
        right' <- guarded (Ty.Not $ ssaBool left') $ genExprSMT right
        return $ liftTermFun2 "cOr" cOr left' right'
      _ -> do
        left'  <- genExprSMT left
        right' <- genExprSMT right
        getBinOp op left' right'
    CUnary op   arg   _ -> getUnaryOp op arg
    CIndex base index _ -> do
      base'  <- genExprSMT base
      index' <- genExprSMT index
      offset <- liftMem $ liftTermFun2M "cIndex" cIndex base' index'
      ssaLoad offset
    CMember struct ident isArrow _ -> do
      e <- genExprSMT struct
      -- If this is a ->, then derefence the left first.
      s <- if isArrow then liftCircify $ getTerm (deref e) else return e
      return $ ssaStructGet (identToVarName ident) s
    CCast decl expr _ -> case decl of
      CDecl specs _ _ -> do
        ty    <- liftCircify $ unwrap <$> baseTypeFromSpecs specs
        expr' <- genExprSMT expr
        return $ liftTermFun "cCast" (cCast ty) expr'
      _ -> error "Expected type in cast"
    CCall fn args _ -> case fn of
      CVar fnIdent _ -> do
        let fnName = identToVarName fnIdent
        actualArgs <- traverse genExprSMT args
        s          <- genSpecialFunction fnName actualArgs
        case s of
          Just r  -> return r
          Nothing -> do
            f     <- getFunction fnName
            retTy <- liftCircify $ unwrap <$> ctype (baseTypeFromFunc f)
                                                    (ptrsFromFunc f)
            liftCircify $ pushFunction fnName (noneIfVoid retTy)
            forM_ (argsFromFunc f) (genDeclSMT FnArg)
            let
              formalArgs =
                map (SLVar . identToVarName)
                  $ concatMap
                      (\case
                        CDecl _ decls _ -> map
                          (\(Just dec, _, _) ->
                            let mName = identFromDeclr dec
                            in  fromMaybe
                                  (error "Expected identifier in decl")
                                  mName
                          )
                          decls
                        _ -> error "Missing case in formalArgs"
                      )
                  $ argsFromFunc f
            unless (length formalArgs == length actualArgs)
              $  error
              $  "Wrong arg count: "
              ++ show expr
            liftCircify $ forM_ (zip formalArgs actualArgs) (uncurry argAssign)
            let body = bodyFromFunc f
            case body of
              CCompound{} -> genStmtSMT body
              _ -> error "Expected C statement block in function definition"
            returnValue <- liftCircify popFunction
            return $ Base $ fromMaybe
              (error "Getting the return value of a void fn")
              returnValue
      _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
    CCond cond mTrueBr falseBr _ -> do
      cond'  <- genExprSMT cond
      true'  <- maybe (return cond') genExprSMT mTrueBr
      false' <- genExprSMT falseBr
      return $ liftTermFun3 "cCond" cCond cond' true' false'
    CSizeofExpr e _ -> do
      -- Evaluate in false context, to get type, but avoid side-effects
      e' <- guarded (Ty.BoolLit False) (genExprSMT e)
      let bits = case e' of
            Base c   -> numBits (cType c)
            RefVal{} -> numBits $ Ptr32 U8
      return $ Base $ cIntLit U32 (toInteger $ bits `div` 8)
    CSizeofType decl _ -> do
      ty <- liftCircify $ unwrap <$> cDeclToType decl
      return $ Base $ cIntLit U32 (toInteger $ numBits ty `div` 8)
    _ -> error $ unwords ["We do not support", show expr, "right now"]


getUnaryOp :: CUnaryOp -> CExpr -> C CSsaVal
getUnaryOp op arg = case op of
  _ | isIncDec op -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    let one = Base $ cIntLit (ssaType rval) 1
    let new = liftTermFun2 (show op) (if isDec op then cSub else cAdd) rval one
    _ <- genAssign lval new
    return $ if isPre op then new else rval
  CIndOp -> do
    l <- genExprSMT arg
    ssaLoad l
  CPlusOp -> error $ unwords ["Do not understand:", show op]
  CMinOp  -> liftTermFun "cNeg" cNeg <$> genExprSMT arg
  CCompOp -> liftTermFun "cBitNot" cBitNot <$> genExprSMT arg
  CNegOp  -> liftTermFun "cNot" cNot <$> genExprSMT arg
  CAdrOp  -> genRefSMT arg
  _       -> error $ unwords [show op, "not supported"]
 where
  isIncDec o = o `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp]
  isDec o = o `elem` [CPreDecOp, CPostDecOp]
  isPre o = o `elem` [CPreDecOp, CPreDecOp]

getBinOp :: CBinaryOp -> CSsaVal -> CSsaVal -> C CSsaVal
getBinOp op left right =
  let f = case op of
        CMulOp -> cMul
        CDivOp -> cDiv
        CRmdOp -> cRem
        CAddOp -> cAdd
        CSubOp -> cSub
        CShlOp -> cShl
        CShrOp -> cShr
        CLeOp  -> cLt
        CGrOp  -> cGt
        CLeqOp -> cLe
        CGeqOp -> cGe
        CEqOp  -> cEq
        CNeqOp -> cNe
        CAndOp -> cBitAnd
        CXorOp -> cBitXor
        COrOp  -> cBitOr
        CLndOp -> cAnd
        CLorOp -> cOr
  in  return $ liftTermFun2 (show op) f left right

-- | Assign operation
-- eg x += 1
-- aka x = x + 1
genAssignOp :: CAssignOp -> CLVal -> CSsaVal -> C CSsaVal
genAssignOp op l r = case op of
  CAssignOp -> genAssign l r
  _ ->
    let f = case op of
          CMulAssOp -> cMul
          CAddAssOp -> cAdd
          CSubAssOp -> cSub
          CShlAssOp -> cShl
          CShrAssOp -> cShr
          CAndAssOp -> cBitAnd
          CXorAssOp -> cBitXor
          COrAssOp  -> cBitOr
          o         -> error $ unwords ["Cannot handle", show o]
    in  do
          lvalue <- evalLVal l
          genAssign l (liftTermFun2 (show op) f lvalue r)

---
--- Statements
---

genStmtSMT :: CStat -> C ()
genStmtSMT stmt = do
  liftLog $ logIfM "stmt" $ do
    t <- liftIO $ nodeText stmt
    return $ "Stmt: " ++ t
  case stmt of
    CCompound _ items _ -> do
      liftCircify enterLexScope
      forM_ items $ \case
        CBlockStmt stmt -> genStmtSMT stmt
        CBlockDecl decl -> void $ genDeclSMT Local decl
        CNestedFunDef{} -> error "Nested function definitions not supported"
      liftCircify exitLexScope
    CExpr e _ -> when (isJust e) $ void $ genExprSMT $ fromJust e
    CIf cond trueBr falseBr _ -> do
      trueCond <- ssaBool <$> genExprSMT cond
      -- Guard the true branch with the true condition
      guarded trueCond $ genStmtSMT trueBr
      -- Guard the false branch with the false condition
      forM_ falseBr $ \br -> guarded (Ty.Not trueCond) $ genStmtSMT br
    CFor init check incr body _ -> do
      case init of
        Left  (Just expr) -> void $ genExprSMT expr
        Right decl        -> void $ genDeclSMT Local decl
        _                 -> return ()
      -- Make a guard on the bound to guard execution of the loop
      -- Execute up to the loop bound
      bound <- getLoopBound
      replicateM_ bound $ do
        test <- genExprSMT $ fromMaybe (error "Missing test in for-loop") check
        liftCircify $ pushGuard (ssaBool test)
        genStmtSMT body
        -- increment the variable
        forM_ incr $ \inc -> genExprSMT inc
      replicateM_ bound (liftCircify popGuard)
      -- TODO: assert end
    CWhile check body isDoWhile _ -> do
      bound <- getLoopBound
      let addGuard = genExprSMT check >>= liftCircify . pushGuard . ssaBool
      replicateM_ bound $ do
        unless isDoWhile addGuard
        genStmtSMT body
        when isDoWhile addGuard
      replicateM_ bound (liftCircify popGuard)
    CReturn expr _ -> when (isJust expr) $ do
      toReturn <- genExprSMT $ fromJust expr
      liftCircify $ doReturn $ ssaValAsTerm "return" toReturn
    CLabel _ inner _ _ -> genStmtSMT inner
    _                  -> do
      text <- liftIO $ nodeText stmt
      error $ unlines ["Unsupported:", text]

-- Kind of declaration
data DeclType = FnArg -- ^ Argument to a called function. Internally defined.
              | Local -- ^ Local variable. Not defined if uninitialized.
              | EntryFnArg -- ^ Top level function argument. Externally defined.
              deriving (Eq)

-- | Returns the names of all declared variables, and their types
-- @isInput@: whether the declared variables are inputs to the constraint system (vs. witnesses)
genDeclSMT :: DeclType -> CDecl -> C [(String, Type)]
genDeclSMT dType d@(CDecl specs decls _) = do
  liftLog $ logIf "decls" "genDeclSMT:"
  liftLog $ logIfM "decls" $ liftIO $ nodeText d
  -- At the top level, we ignore types we don't understand.
  skipBadTypes <- liftCircify $ gets (null . callStack)
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        isStorageSpec firstSpec && isTypedef (storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  -- Even for not declarators, process the type. It may be a struct that needs to be recorded!
  when (null decls) $ void $ liftCircify $ baseTypeFromSpecs baseType

  ms <- forM decls $ \(Just dec, mInit, _) -> do
    let mName   = identFromDeclr dec
        ident   = fromMaybe (error "Expected identifier in decl") mName
        name    = identToVarName ident
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then do
        ty <- typedefSMT ident baseType ptrType
        return $ ("TYPEDEF", ) <$> ty
      else do
        ty <- case mInit of
          Just init -> do
            ty <- liftCircify $ ctype baseType ptrType
            case ty of
              Left  err -> if skipBadTypes then return Nothing else error err
              Right ty  -> do
                rhs <- genInitSMT ty init
                liftCircify $ declareInitVar name ty rhs
                return $ Just ty
          Nothing -> do
            mTy <- declareVarSMT (dType == EntryFnArg) ident baseType ptrType
            when (not skipBadTypes || isRight mTy) $ do
              lhs <- genVarSMT ident
              whenM (gets findUB)
                $  when (dType /= FnArg)
                $  liftAssert
                $  Assert.assert
                $ Ty.Eq (udef $ ssaValAsTerm "undef settting in genDeclSMT" lhs)
                $  Ty.BoolLit
                $  dType
                == Local
            return $ either (const Nothing) Just mTy
        return $ (name, ) <$> ty
  return $ catMaybes ms
genDeclSMT _ _ = error "Missing case in genDeclSMT"

genInitSMT :: Type -> CInit -> C CSsaVal
genInitSMT ty i = case (ty, i) of
  (_, CInitExpr e _) -> do
    t <- genExprSMT e
    return $ case t of
      Base c   -> Base $ cCast ty c
      RefVal{} -> t
  (Array _ innerTy, CInitList is _) -> do
    values <- forM is $ \(_, i) -> genInitSMT innerTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in arrays") values
    liftMem $ Base <$> cArrayLit innerTy cvals
  (Struct fields, CInitList is _) -> do
    values <- forM (zip fields is) $ \((_, fTy), (_, i)) -> genInitSMT fTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in structs") values
    liftMem $ Base <$> cStructLit ty cvals
  _ -> error $ unwords ["Cannot initialize type", show ty, "from", show i]

---
--- High level codegen (translation unit, etc)
---

-- Returns the variable names corresponding to inputs and the return
genFunDef :: CFunDef -> C ([String], Maybe String)
genFunDef f = do
  -- Declare the function and get the return type
  let name = nameFromFunc f
      ptrs = ptrsFromFunc f
      tys  = baseTypeFromFunc f
  retTy <- liftCircify $ unwrap <$> ctype tys ptrs
  liftCircify $ pushFunction name $ noneIfVoid retTy
  -- Declare the arguments and execute the body
  inputNamesAndTys <- join <$> forM (argsFromFunc f) (genDeclSMT EntryFnArg)
  let inputNames = map fst inputNamesAndTys
  fullInputNames <- map ssaVarAsString
    <$> forM inputNames (liftCircify . getSsaVar . SLVar)

  let body = bodyFromFunc f
  case body of
    CCompound{} -> genStmtSMT body
    _           -> error "Expected C statement block in function definition"
  returnValue <- liftCircify popFunction
  forM_ returnValue $ liftCircify . makePublic "return" . fromJust . asVar
  whenM (gets findUB) $ forM_ returnValue $ \r -> bugIf $ udef r
  return (fullInputNames, fromJust . asVar <$> returnValue)

genAsm :: CStringLiteral a -> C ()
genAsm = undefined

registerFns :: [CExtDecl] -> C ()
registerFns decls = forM_ decls $ \case
  CFDefExt f    -> registerFunction (nameFromFunc f) f
  CDeclExt d    -> void $ genDeclSMT Local d
  CAsmExt asm _ -> genAsm asm

codegenAll :: CTranslUnit -> C ()
codegenAll (CTranslUnit decls _) = do
  registerFns decls
  forM_ decls $ \case
    CDeclExt decl -> void $ genDeclSMT Local decl
    CFDefExt fun  -> void $ genFunDef fun
    CAsmExt asm _ -> genAsm asm

findFn :: String -> [CExtDecl] -> CFunDef
findFn name decls =
  let nameFnPair (CFDefExt f) = [(nameFromFunc f, f)]
      nameFnPair _            = []
      namesToFns = Map.fromList $ concatMap nameFnPair decls
  in  fromMaybe
        (  error
        $  "No function `"
        ++ name
        ++ "`. Available functions: {"
        ++ intercalate ", " (Map.keys namesToFns)
        ++ "}."
        )
        (namesToFns Map.!? name)

codegenFn :: CTranslUnit -> String -> C ([String], Maybe String)
codegenFn (CTranslUnit decls _) name = do
  registerFns decls
  genFunDef (findFn name decls)

cSetInputs :: Maybe InMap -> Bool -> String -> Maybe VarName -> Type -> Mem ()
cSetInputs inputs findBugs smtName mUserName ty = do
  let inMap    = fromMaybe (error "Accessing missing inputs!") inputs
      -- term provided under the user name
      userTerm = do
        userName <- mUserName
        parseVar inMap False userName ty
      -- term provided under the smt name. Undef, since internal?
      smtTerm     = parseVar inMap True smtName ty
      defaultTerm = cZeroInit ty
      term        = fromMaybe defaultTerm (userTerm <|> smtTerm)
  liftLog $ logIf "inputs" $ unwords
    [ "C setting input:"
    , show smtName
    , show mUserName
    , show userTerm
    , show smtTerm
    , show defaultTerm
    , show term
    ]
  liftAssert $ cSetValues findBugs smtName term


cLangDef :: Maybe (Map.Map [Ext] Integer) -> Bool -> LangDef Type CTerm
cLangDef inputs findBugs = LangDef { declare   = cDeclVar findBugs
                                   , assign    = cCondAssign findBugs
                                   , setValues = cSetValues findBugs
                                   , setInputs = cSetInputs inputs findBugs
                                   }

runC
  :: Maybe InMap
  -> Bool
  -> C a
  -> Assert.Assert (a, CircifyState Type CTerm, MemState)
runC inMap findBugs c = do
  when (isJust inMap) Assert.initValues
  let (C act) = cfgFromEnv >> c
  (((x, _), circState), memState) <-
    runCodegen (cLangDef inMap findBugs) $ runStateT act (emptyCState findBugs)
  return (x, circState, memState)

evalC :: Maybe InMap -> Bool -> C a -> Assert.Assert a
evalC inMap findBugs act = do
  (r, _, _) <- runC inMap findBugs act
  return r

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
checkFn
  :: CTranslUnit -> String -> Cfg ([String], Maybe (Map.Map String Ty.Val))
checkFn tu name = do
  (ins, assertions) <- Assert.runAssert $ evalC Nothing True $ do
    (ins, _) <- codegenFn tu name
    assertBug
    return ins
  model <- liftIO $ Ty.evalZ3Model $ Ty.BoolNaryExpr
    Ty.And
    (Assert.asserted assertions)
  return (ins, if Map.null model then Nothing else Just model)

evalFn :: Bool -> CTranslUnit -> String -> Cfg (Map.Map String Ty.Val)
evalFn findBug tu name = do
  -- TODO: inputs?
  assertions <- Assert.execAssert $ evalC Nothing findBug $ do
    _ <- codegenFn tu name
    when findBug assertBug
  liftIO $ Ty.evalZ3Model $ Ty.BoolNaryExpr Ty.And (Assert.asserted assertions)

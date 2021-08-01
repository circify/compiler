{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codegen.Zokrates.Term
  ( Term(..)
  -- Binary
  , zAdd
  , zSub
  , zMul
  , zDiv
  , zShl
  , zShr
  , zPow
  , zBitAnd
  , zBitOr
  , zBitXor
  , zGe
  , zGt
  , zLt
  , zLe
  , zNe
  , zEq
  , zAnd
  , zOr
  -- Unary
  , zNeg
  , zNot
  -- Ternary
  , zCond
  -- Typing & casting
  , zBool
  , zConstInt
  , zType
  -- Array
  , zSpread
  , zSlice
  , zArray
  , zArrayGet
  , zArraySet
  -- Struct
  , zFieldGet
  , zFieldSet
  -- Infra
  , zTermVars
  , zTermPublicize
  -- Built-ins
  , zU32fromBits
  , zU32toBits
  , zFieldtoBits
  , naryReturnTerm
  )
where

import           Control.Monad
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify                ( Embeddable(..) )
import qualified Codegen.Zokrates.Type         as T
import           Codegen.LangVal                ( InMap
                                                , setInputFromMap
                                                )
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe, isJust )
import qualified Data.Map.Strict               as Map
import           Data.List                      ( group )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import qualified IR.SMT.TySmt                  as S
import qualified IR.SMT.TySmt.Alg              as SAlg
--import qualified IR.SMT.TySmt.DefaultMap       as DMap
import qualified IR.SMT.Assert                 as Assert
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Util.Log

data Term n = BitInt Int S.TermDynBv
            | Field  (S.TermPf n)
            | Bool   S.TermBool
            | Array Int [Term n]
            | FieldArray Int (S.TermArray (S.PfSort n) (S.PfSort n))
            | BoolArray Int (S.TermArray (S.PfSort n) S.BoolSort)
            | BitIntArray Int Int (S.TermArray (S.PfSort n) S.DynBvSort)
            | Struct String (Map.Map String (Term n))
            deriving (Show)

wrapBin
  :: forall n
   . String
  -> Maybe (S.TermDynBv -> S.TermDynBv -> S.TermDynBv)
  -> Maybe (S.TermPf n -> S.TermPf n -> S.TermPf n)
  -> Maybe (S.TermBool -> S.TermBool -> S.TermBool)
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapBin name fI fF fB a b = case (a, b, fI, fF, fB) of
  (BitInt w0 a', BitInt w1 b', Just f, _, _) | w0 == w1 ->
    Right $ BitInt w0 $ f a' b'
  (Field a', Field b', _, Just f, _) -> Right $ Field $ f a' b'
  (Bool a', Bool b', _, _, Just f) -> Right $ Bool $ f a' b'
  _ -> Left $ unwords
    ["Cannot perform operation", show name, "on\n", show a, "and\n", show b]

wrapBinPred
  :: forall n
   . KnownNat n
  => String
  -> Maybe (S.TermDynBv -> S.TermDynBv -> S.TermBool)
  -> Maybe (S.TermPf n -> S.TermPf n -> S.TermBool)
  -> Maybe (S.TermBool -> S.TermBool -> S.TermBool)
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapBinPred name fI fF fB a b = case (a, b, fI, fF, fB) of
  (BitInt w0 a', BitInt w1 b', Just f, _, _) | w0 == w1 ->
    Right $ Bool $ f a' b'
  (Field a' , Field b' , _, Just f, _     ) -> Right $ Bool $ f a' b'
  (Bool  a' , Bool b'  , _, _     , Just f) -> Right $ Bool $ f a' b'
  --- HACK
  (Array _ a, Array _ b, _, _     , _     ) -> do
    bools <- (mapM (uncurry zEq) $ zip a b) >>= mapM zBool
    Right $ Bool $ S.BoolNaryExpr S.And bools
  _ -> Left $ unwords
    ["Cannot perform operation", show name, "on", show a, "and", show b]

zAdd, zSub, zMul, zDiv, zShl, zShr, zPow, zBitAnd, zBitOr, zBitXor, zGe, zGt, zLt, zLe, zNe, zEq, zAnd, zOr
  :: KnownNat n => Term n -> Term n -> Either String (Term n)


-- Binarize an nary fn
bin :: ([a] -> t) -> a -> a -> t
bin f x y = f [x, y]

zAdd = wrapBin "+"
               (Just $ bin $ S.mkDynBvNaryExpr S.BvAdd)
               (Just $ bin (S.PfNaryExpr S.PfAdd))
               Nothing
zSub = wrapBin
  "-"
  (Just $ S.mkDynBvBinExpr S.BvSub)
  (Just $ \a b -> S.PfNaryExpr S.PfAdd [a, S.PfUnExpr S.PfNeg b])
  Nothing
zMul = wrapBin "*"
               (Just $ bin $ S.mkDynBvNaryExpr S.BvMul)
               (Just $ bin (S.PfNaryExpr S.PfMul))
               Nothing
zDiv = wrapBin
  "/"
  (Just $ S.mkDynBvBinExpr S.BvUdiv)
  (Just $ \a b -> S.PfNaryExpr S.PfMul [a, S.PfUnExpr S.PfRecip b])
  Nothing

zPow a b = do
  base <- zConstInt a
  case (b, base) of
    (Field f, 2) -> Right (Field (S.PfUnExpr S.PfTwoPow f))
    _            -> Left "bad power"
zBitAnd = wrapBin "&" (Just $ bin $ S.mkDynBvNaryExpr S.BvAnd) Nothing Nothing
zBitOr = wrapBin "|" (Just $ bin $ S.mkDynBvNaryExpr S.BvOr) Nothing Nothing
zBitXor = wrapBin "^" (Just $ bin $ S.mkDynBvNaryExpr S.BvXor) Nothing Nothing
zAnd = wrapBin "&&" Nothing Nothing (Just $ bin (S.BoolNaryExpr S.And))
zOr = wrapBin "||" Nothing Nothing (Just $ bin (S.BoolNaryExpr S.Or))
--zEq = wrapBinPred "==" (Just S.mkEq) (Just S.mkEq) (Just S.mkEq)
--zNe = wrapBinPred "!=" (Just ne) (Just ne) (Just ne)
--
--ne :: S.SortClass s => S.Term s -> S.Term s -> S.TermBool
--ne x y = S.Not $ S.mkEq x y
zGe = wrapBinPred ">=" (Just $ S.mkDynBvBinPred S.BvUge) Nothing Nothing
zGt = wrapBinPred ">" (Just $ S.mkDynBvBinPred S.BvUgt) Nothing Nothing
zLe = wrapBinPred "<=" (Just $ S.mkDynBvBinPred S.BvUle) Nothing Nothing
zLt = wrapBinPred "<"
                  (Just $ S.mkDynBvBinPred S.BvUlt)
                  (Just $ S.PfBinPred S.PfLt)
                  Nothing



zNe a b = zEq a b >>= zNot
zEq a b = case (a, b) of
  (BitInt w0 a', BitInt w1 b') | w0 == w1 ->
    Right $ Bool $ S.mkEq a' b'
  (Field a' , Field b' ) -> Right $ Bool $ S.mkEq a' b'
  (Bool  a' , Bool b'  ) -> Right $ Bool $ S.mkEq a' b'
  --- HACK
  (Array _ a, Array _ b) -> do
    bools <- (mapM (uncurry zEq) $ zip a b) >>= mapM zBool
    Right $ Bool $ S.BoolNaryExpr S.And bools
  (BoolArray _ xs, BoolArray _ ys) -> Right $ Bool $ S.mkEq xs ys
  (FieldArray _ xs, FieldArray _ ys) -> Right $ Bool $ S.mkEq xs ys
  (BitIntArray _ _ xs, BitIntArray _ _ ys) -> Right $ Bool $ S.mkEq xs ys
  _ -> Left $ unwords ["Cannot perform operation", "==", "on", show a, "and", show b]

wrapShift
  :: KnownNat n
  => String
  -> S.BvBinOp
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapShift name op a b = do
  amt <- toInteger <$> zConstInt b
  case a of
    (BitInt w0 a') ->
      Right $ BitInt w0 $ S.mkDynBvBinExpr op a' $ Mem.bvNum False w0 amt
    _ -> Left $ unwords
      ["Cannot perform operation", show name, "on\n", show a, "and\n", show b]
zShl = wrapShift "<<" S.BvShl
zShr = wrapShift ">>" S.BvLshr

wrapUn
  :: forall n
   . String
  -> Maybe (S.TermDynBv -> S.TermDynBv)
  -> Maybe (S.TermPf n -> S.TermPf n)
  -> Maybe (S.TermBool -> S.TermBool)
  -> Term n
  -> Either String (Term n)
wrapUn name fI fF fB a = case (a, fI, fF, fB) of
  (BitInt w a', Just f, _, _) -> Right $ BitInt w $ f a'
  (Field a', _, Just f, _) -> Right $ Field $ f a'
  (Bool a', _, _, Just f) -> Right $ Bool $ f a'
  _ -> Left $ unwords ["Cannot perform operation", show name, "on", show a]

zNeg, zNot :: KnownNat n => Term n -> Either String (Term n)
zNeg = wrapUn "unary-"
              (Just $ S.mkDynBvUnExpr S.BvNeg)
              (Just $ S.PfUnExpr S.PfNeg)
              Nothing
zNot = wrapUn "!" (Just $ S.mkDynBvUnExpr S.BvNot) Nothing (Just S.Not)

type Field n = (String, Term n)
zIte :: KnownNat n => S.TermBool -> Term n -> Term n -> Either String (Term n)
zIte c a b = case (a, b) of
  (Bool  x, Bool y )                    -> Right $ Bool $ S.mkIte c x y
  (Field x, Field y)                    -> Right $ Field $ S.mkIte c x y
  (BitInt w0 x, BitInt w1 y) | w0 == w1 -> Right $ BitInt w0 $ S.mkIte c x y
  (Array w0 x, Array w1 y) | w0 == w1   -> Array w0 <$> zipWithM (zIte c) x y
  (FieldArray w0 x, FieldArray w1 y) | w0 == w1 ->
    Right $ FieldArray w0 $ S.mkIte c x y
  (BoolArray w0 x, BoolArray w1 y) | w0 == w1 ->
    Right $ BoolArray w0 $ S.mkIte c x y
  (BitIntArray w0 l0 x, BitIntArray w1 l1 y) | l0 == l1 && w0 == w1 ->
    Right $ BitIntArray w0 l0 $ S.mkIte c x y
  (Struct n0 xs, Struct n1 ys) | n0 == n1 -> do
    ps <- zipWithM zipField (Map.toAscList xs) (Map.toAscList ys)
    return $ Struct n0 $ Map.fromList ps
   where
    zipField :: KnownNat n => Field n -> Field n -> Either String (Field n)
    zipField (f0, x) (f1, y) = if f0 == f1
      then (f0, ) <$> zIte c x y
      else Left $ unwords ["Field mismatch in ite:", show f0, "v", show f1]
  _ -> Left $ unwords ["Cannot perform ITE on", show a, "and", show b]

zBool :: KnownNat n => Term n -> Either String S.TermBool
zBool t = case t of
  Bool b -> Right b
  _      -> Left $ show t ++ " is not bool"

zConstInt :: KnownNat n => Term n -> Either String Int
zConstInt t = case t of
  BitInt _ (S.DynBvLit b) -> Right $ fromIntegral $ Bv.nat b
  Field (S.IntToPf (S.IntLit f)) -> Right $ fromIntegral f
  _ -> Left $ show t ++ " is not a constant integer"

zCond :: KnownNat n => Term n -> Term n -> Term n -> Either String (Term n)
zCond c t f = do
  c' <- zBool c
  zIte c' t f

zSlice
  :: KnownNat n
  => Term n
  -> Maybe Int
  -> Maybe Int
  -> Assert.Assert (Either String (Term n))
zSlice t a b = case t of
  Array d items ->
    let a' = fromMaybe 0 a
        w  = fromMaybe d b - a'
    in  return $ Right $ Array w $ take w $ drop a' items
  FieldArray d _items -> do
    let a' = fromMaybe 0 a
        e  = fromMaybe d b - 1
        ts = mapM (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t) [a' .. e]
    either (return . Left) zArray ts
  BitIntArray _ d _items -> do
    let a' = fromMaybe 0 a
        e  = fromMaybe d b - 1
        ts = mapM (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t) [a' .. e]
    either (return . Left) zArray ts
  BoolArray d _items -> do
    let a' = fromMaybe 0 a
        e  = fromMaybe d b - 1
        ts = mapM (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t) [a' .. e]
    either (return . Left) zArray ts
  _ -> return $ Left $ unwords ["Cannot slice", show t]

zSpread :: KnownNat n => Term n -> Either String [Term n]
zSpread t = case t of
  Array      _ items  -> Right items
  FieldArray d _items -> mapM
    (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t)
    [0 .. d - 1]
  BoolArray d _items -> mapM
    (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t)
    [0 .. d - 1]
  BitIntArray _ d _items -> mapM
    (\i -> zArrayGet (Field $ S.IntToPf $ S.IntLit $ toInteger i) t)
    [0 .. d - 1]
  _ -> Left $ unwords ["Cannot spread", show t]

zType :: KnownNat n => Term n -> T.Type
zType t = case t of
  BitInt i _        -> T.Uint i
  Bool  _           -> T.Bool
  Field _           -> T.Field
  Struct     n fs   -> T.Struct n $ Map.map zType fs
  FieldArray d _    -> T.Array d T.Field
  BoolArray  d _    -> T.Array d T.Bool
  BitIntArray w d _ -> T.Array d (T.Uint w)
  Array d xs ->
    let tys = map zType xs
    in  case length (group tys) of
          0 -> error "Cannot get type of empty array"
          1 -> T.Array d (head tys)
          _ -> error $ "Different types in " ++ show t

zFieldGet :: KnownNat n => String -> Term n -> Either String (Term n)
zFieldGet f t = case t of
  Struct _ fs | Map.member f fs -> Right $ fs Map.! f
  _ -> Left $ unwords ["Cannot find field", show f, "in", show t]

zFieldSet :: KnownNat n => String -> Term n -> Term n -> Either String (Term n)
zFieldSet f v t = case t of
  Struct n fs | Map.member f fs -> Right $ Struct n $ Map.adjust (const v) f fs
  _ -> Left $ unwords ["Cannot find field", show f, "in", show t]

zArrayGet :: KnownNat n => Term n -> Term n -> Either String (Term n)
zArrayGet i a = case (i, a) of
  -- TODO bounds check index
  (Field i', Array d xs) | d > 0 ->
    foldM (\acc (j, v) -> zIte (S.mkEq i' (S.IntToPf $ S.IntLit j)) v acc)
          (head xs)
      $ zip [1 ..]
      $ tail xs
  (Field i', FieldArray d xs) | d > 0 -> Right $ Field $ S.mkSelect xs i'
  (Field i', BitIntArray w d xs) | d > 0 -> Right $ BitInt w $ S.mkSelect xs i'
  (Field i', BoolArray d xs) | d > 0 -> Right $ Bool $ S.mkSelect xs i'
  _ -> Left $ unwords ["Cannot get index", show i, "from array", show a]

zArraySet
  :: forall n
   . KnownNat n
  => Term n
  -> Term n
  -> Term n
  -> Assert.Assert (Either String (Term n))
zArraySet i v a = case (i, a, v) of
  -- TODO bounds check index?? Less clear here.
  (Field i', Array d xs, _) | d > 0 -> return
    (   Array d
    <$> zipWithM (\j -> zIte (S.mkEq i' (S.IntToPf $ S.IntLit j)) v) [0 ..] xs
    )
  (Field i', FieldArray d xs, Field v') | d > 0 -> do
    let store = S.mkStore xs i' v'
    v <- Assert.freshVar "array_set"
                         (S.SortArray (S.SortPf modulus) (S.SortPf modulus))
    Assert.assign v store
    return $ Right $ FieldArray d v
  (Field i', BoolArray d xs, Bool v') | d > 0 -> do
    let store = S.mkStore xs i' v'
    v <- Assert.freshVar "array_set" (S.SortArray (S.SortPf modulus) S.SortBool)
    Assert.assign v store
    return $ Right $ BoolArray d v
  (Field i', BitIntArray w d xs, BitInt w' v') | w == w' && d > 0 -> do
    let store = S.mkStore xs i' v'
    v <- Assert.freshVar "array_set"
                         (S.SortArray (S.SortPf modulus) (S.SortBv w))
    Assert.assign v store
    return $ Right $ BitIntArray w d v
  _ ->
    return $ Left $ unwords ["Cannot set index", show i, "from array", show a]
  where modulus = natVal (Proxy @n)

zArray
  :: forall n
   . KnownNat n
  => [Term n]
  -> Assert.Assert (Either String (Term n))
zArray ts = if length (group $ map zType ts) < 2
  then case zType (head ts) of
    T.Field -> do
      let base = FieldArray l $ S.ConstArray l sortPf (S.IntToPf (S.IntLit 0))
      foldM (\acc (i, term) -> andThen (zArraySet (flit i) term) acc)
            (Right base)
            (zip [0 ..] ts)
    T.Bool -> do
      let base = BoolArray l $ S.ConstArray l sortPf (S.BoolLit False)
      foldM (\acc (i, term) -> andThen (zArraySet (flit i) term) acc)
            (Right base)
            (zip [0 ..] ts)
    T.Uint w -> do
      let base =
            BitIntArray w l $ S.ConstArray l sortPf (S.DynBvLit $ Bv.zeros w)
      foldM (\acc (i, term) -> andThen (zArraySet (flit i) term) acc)
            (Right base)
            (zip [0 ..] ts)
    _ -> return $ Right $ Array l ts
  else return $ Left $ unwords ["Cannot build array from", show ts]
 where
  l       = length ts
  sortPf  = S.SortPf (natVal (Proxy @n))
  andThen = either (return . Left)
  flit    = Field . S.IntToPf . S.IntLit

--   The first name is required, and is an SMT name that should be a prefix of all generated SMT variables
--   The second name is optional, and is a user-visible name.
--   If present, this varaiable is user-visible, and if values are being
--   computed, the @declare@ function is responsible for getting its value
--   from the user.
zDeclare
  :: forall n
   . KnownNat n
  => Maybe InMap
  -> T.Type
  -> String
  -> Maybe String
  -> Mem.Mem (Term n)
zDeclare inputs ty name mUserName = case ty of
  T.Uint w -> declBase (S.ValDynBv . Bv.bitVec w)
                       (S.ValDynBv $ Bv.zeros w)
                       (S.SortBv w)
                       (BitInt w)
  T.Field -> declBase S.ValPf (S.ValPf 0) (S.SortPf $ natVal (Proxy @n)) Field
  T.Bool  -> declBase (S.ValBool . (/= 0)) (S.ValBool False) S.SortBool Bool
--  T.Array s T.Field ->
--    let idxSort = S.SortPf $ natVal (Proxy @n)
--        arrSort = S.SortArray idxSort idxSort
--        defaultValue = S.ValArray (DMap.emptyWithDefault (S.ValPf 0))
--        rec i = zDeclare inputs inner (aName i name) (aName i <$> mUserName)
--        Array s <$> forM [0 .. (s - 1)] rec
--    in  declBase undefined defaultValue arrSort (FieldArray s)
  T.Array s inner ->
    let rec i = zDeclare inputs inner (aName i name) (aName i <$> mUserName)
    in  do
          terms <- forM [0 .. (s - 1)] rec
          logIf "entry" $ "array terms: " ++ show terms
          either error id <$> Assert.liftAssert (zArray terms)
  T.Struct n fs ->
    let rec (f, t) =
            (f, ) <$> zDeclare inputs t (sName f name) (sName f <$> mUserName)
    in  Struct n . Map.fromList <$> forM (Map.toList fs) rec
 where
  declBase
    :: S.SortClass s
    => (Integer -> S.Value s)
    -> S.Value s
    -> S.Sort
    -> (S.Term s -> Term n)
    -> Mem.Mem (Term n)
  declBase parse default_ sort toTerm = Assert.liftAssert $ do
    logIf "decl" $ "declBase: " ++ name ++ " " ++ show mUserName
    t <- Assert.newVar name sort
    when (isJust mUserName) $ Assert.publicize name
    setInputFromMap inputs parse default_ name mUserName
    return $ toTerm t

-- | Create a new set of variables for @term@, which have variable names
-- prefixed by @name@
zAlias :: forall n . KnownNat n => String -> Term n -> Assert.Assert (Term n)
zAlias name term = case term of
  Bool b      -> base S.SortBool b Bool
  BitInt w i  -> base (S.SortBv w) i (BitInt w)
  Field f     -> base (S.SortPf $ natVal (Proxy @n)) f Field
  Struct n fs -> Struct n . Map.fromList <$> forM
    (Map.toList fs)
    (\(f, t) -> (f, ) <$> zAlias (sName f name) t)
  FieldArray w xs ->
    let mod = natVal (Proxy @n)
    in  base (S.SortArray (S.SortPf mod) (S.SortPf mod)) xs (FieldArray w)
  BoolArray w xs ->
    let mod = natVal (Proxy @n)
    in  base (S.SortArray (S.SortPf mod) S.SortBool) xs (BoolArray w)
  BitIntArray w l xs ->
    let mod = natVal (Proxy @n)
    in  base (S.SortArray (S.SortPf mod) (S.SortBv w)) xs (BitIntArray w l)
  Array n xs -> Array n <$> zipWithM (\i -> zAlias (aName i name)) [0 ..] xs
 where
  base
    :: S.SortClass s
    => S.Sort
    -> S.Term s
    -> (S.Term s -> Term n)
    -> Assert.Assert (Term n)
  base sort term toTerm = do
    v <- Assert.newVar name sort
    Assert.assign v term
    return $ toTerm v

-- | Array index name
aName :: Int -> String -> String
aName idx base = base ++ "_" ++ show idx
-- | Struct field name
sName :: String -> String -> String
sName field base = base ++ "_" ++ field

zAssign :: KnownNat n => T.Type -> String -> Term n -> Mem.Mem (Term n, Term n)
zAssign ty name term = if zType term == ty
  then Assert.liftAssert $ (, term) <$> zAlias name term
  else error $ unwords ["Type mismatch", show ty, "v.", show term]

zSetValues :: KnownNat n => String -> Term n -> Assert.Assert ()
zSetValues name t = do
  logIf "values" $ "Setting " ++ show name ++ " to " ++ show t
  case t of
    Bool b            -> Assert.evalAndSetValue name b
    BitInt _ i        -> Assert.evalAndSetValue name i
    Field f           -> Assert.evalAndSetValue name f
    Array _ xs -> zipWithM_ (\i x -> zSetValues (aName i name) x) [0 ..] xs
    FieldArray _ _    -> error "nyi: zSetValues for FieldArray"
    BitIntArray _ _ _ -> error "nyi: zSetValues for BitIntArray _"
    BoolArray _ _     -> error "nyi: zSetValues for BoolArray"
    Struct _ fs ->
      forM_ (Map.toList fs) $ \(f, t) -> zSetValues (sName f name) t

zEvaluate :: KnownNat n => Term n -> Assert.Assert (Maybe (Term n))
zEvaluate t = do
  logIf "values" $ "Evaluating " ++ show t
  case t of
    Bool  b    -> fmap Bool <$> Assert.evalToTerm b
    Field b    -> fmap Field <$> Assert.evalToTerm b
    BitInt w b -> fmap (BitInt w) <$> Assert.evalToTerm b
    Struct ty fs ->
      fmap (Struct ty . Map.fromDistinctAscList) . sequence <$> forM
        (Map.toAscList fs)
        (\(f, t) -> fmap (f, ) <$> zEvaluate t)
    FieldArray w b    -> fmap (FieldArray w) <$> Assert.evalToTerm b
    BitIntArray w l b -> fmap (BitIntArray w l) <$> Assert.evalToTerm b
    BoolArray w b     -> fmap (BoolArray w) <$> Assert.evalToTerm b
    Array     n fs    -> fmap (Array n) . sequence <$> forM fs zEvaluate

zTermVars :: KnownNat n => String -> Term n -> Set.Set String
zTermVars name t = case t of
  Bool b     -> SAlg.vars b
  BitInt _ i -> SAlg.vars i
  Field f    -> SAlg.vars f
  Struct _ l -> Set.unions
    $ Map.mapWithKey (\fName fTerm -> zTermVars (sName fName name) fTerm) l
  FieldArray _ f          -> SAlg.vars f
  BitIntArray _ _ f       -> SAlg.vars f
  BoolArray _       f     -> SAlg.vars f
  Array     _elemTy items -> Set.unions
    $ zipWith (\i fTerm -> zTermVars (aName i name) fTerm) [0 ..] items

zTermPublicize :: forall n . KnownNat n => String -> Term n -> Assert.Assert ()
zTermPublicize name t = case t of
  Bool b         -> base name S.SortBool b
  BitInt w i     -> base name (S.SortBv w) i
  Field f        -> base name (S.SortPf $ natVal (Proxy @n)) f
  FieldArray l f -> do
    let terms = map
          (\i -> S.Select f (S.IntToPf $ S.IntLit $ fromIntegral i))
          ([0 .. (l - 1)])
    forM_ (zip terms [0 ..])
      $ \(t, i) -> base (aName i name) (S.SortPf $ natVal (Proxy @n)) t
  BitIntArray w l f -> do
    let terms = map
          (\i -> S.Select f (S.IntToPf $ S.IntLit $ fromIntegral i))
          ([0 .. (l - 1)])
    forM_ (zip terms [0 ..]) $ \(t, i) -> base (aName i name) (S.SortBv w) t
  BoolArray l f -> do
    let terms = map
          (\i -> S.Select f (S.IntToPf $ S.IntLit $ fromIntegral i))
          ([0 .. (l - 1)])
    forM_ (zip terms [0 ..]) $ \(t, i) -> base (aName i name) S.SortBool t
  Struct _ l ->
    forM_ (Map.toList l) $ \(k, v) -> zTermPublicize (sName k name) v
  Array _elemTy items ->
    forM_ (zip [0 ..] items) $ \(k, v) -> zTermPublicize (aName k name) v
 where
  base :: S.SortClass s => String -> S.Sort -> S.Term s -> Assert.Assert ()
  base name sort term = do
    logIf "return" $ "Publicizing ret part: " ++ show name ++ " = " ++ show term
    v <- Assert.newVar name sort
    Assert.publicize name
    Assert.assign v term

zU32toBits :: KnownNat n => Term n -> Assert.Assert (Either String (Term n))
zU32toBits u32 = case u32 of
  BitInt 32 bv -> zArray $ map (Bool . flip S.mkDynBvExtractBit bv) [0 .. 31]
  _            -> return $ Left $ "Cannot call (u32) to_bits on " ++ show u32

zU32fromBits :: KnownNat n => Term n -> Either String (Term n)
zU32fromBits array = case array of
  Array 32 bits -> do
    bits' <- forM bits $ \case
      Bool b -> Right b
      t      -> Left $ "Non-bit " ++ show t
    return $ BitInt 32 $ foldl1 S.mkDynBvConcat $ map S.BoolToDynBv bits'
  BoolArray 32 arr -> do
    bits' <- forM [0..31] $ \i -> Right $ S.mkSelect arr (S.IntToPf $ S.IntLit i)
    return $ BitInt 32 $ foldl1 S.mkDynBvConcat $ map S.BoolToDynBv bits'
  _ -> Left $ "Cannot call (u32) from_bits on " ++ show array

zFieldtoBits :: KnownNat n => Term n -> Assert.Assert (Either String (Term n))
zFieldtoBits t = case t of
  Field f ->
    let bv = S.PfToDynBv 254 f
    in  zArray $ map (Bool . flip S.mkDynBvExtractBit bv) [0 .. 253]
  _ -> return $ Left $ "Cannot call (field) unpack on " ++ show t

instance KnownNat n => Embeddable T.Type (Term n) (Maybe InMap) where
  declare   = zDeclare
  ite       = const $ ((.) . (.) . (.)) (return . either error id) zIte
  assign    = const zAssign
  setValues = const zSetValues
  evaluate  = const zEvaluate

naryReturnTerm :: KnownNat n => [Term n] -> Term n
naryReturnTerm ts =
  if length ts > 1
    then Struct T.naryReturnTypeName $ Map.fromList $ zip (show <$> [(0 :: Int) ..]) ts
    else head ts

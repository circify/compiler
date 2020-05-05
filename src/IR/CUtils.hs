{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module IR.CUtils
  ( CTerm(..)
  , CTermData(..)
  , Bitable(..)
  , newVar
  -- Memory Operations
  , cppLoad
  , cppStore
  -- Arith Operations
  , cppBitOr
  , cppBitXor
  , cppBitNot
  , cppBitAnd
  , cppSub
  , cppMul
  , cppAdd
  , cppMin
  , cppMax
  , cppDiv
  , cppRem
  , cppPos
  , cppNeg
  , cppShl
  , cppShr
  -- Logical Operations
  , cppAnd
  , cppOr
  , cppNot
  -- Other
  , cppCond
  , cppAssign
  )
where

import qualified IR.TySmt                      as Ty
import qualified AST.Simple                    as AST
import qualified Targets.SMT.Assert            as Assert
import           Targets.SMT.Assert             ( Assert )
import qualified IR.Memory                     as Mem
import           IR.Memory                      ( Mem )
import           Data.Foldable                 as Fold

-- data CInt = CInt { _cintSigned :: Bool
--                  , _cintBits :: Int
--                  , _cintSmt :: Ty.TermDynBv
--                  }

type Bv = Ty.TermDynBv

class Bitable s where
  nbits :: s -> Int
  serialize :: s -> Bv
  deserialize :: AST.Type -> Bv -> s


data CTermData = CInt Bool Int Bv
               | CBool Ty.TermBool
               | CDouble Ty.TermDouble
               | CPtr AST.Type Bv
               deriving (Show)

ctermDataTy :: CTermData -> AST.Type
ctermDataTy t = case t of
  CInt True 8 _ -> AST.S8
  CInt False 8 _ -> AST.U8
  CInt True 16 _ -> AST.S16
  CInt False 16 _ -> AST.U16
  CInt True 32 _ -> AST.S32
  CInt False 32 _ -> AST.U32
  CInt True 64 _ -> AST.S64
  CInt False 64 _ -> AST.U64
  CInt _ w _ -> error $ unwords ["Invalid int width:", show w]
  CBool {} -> AST.Bool
  CDouble {} -> AST.Double
  CPtr ty _ -> ty

asDouble :: CTermData -> Ty.TermDouble
asDouble (CDouble d) = d
asDouble t           = error $ unwords [show t, "is not a double"]

asInt :: CTermData -> (Bool, Int, Bv)
asInt (CInt s w i) = (s, w, i)
asInt t            = error $ unwords [show t, "is not an integer"]

asBool :: CTermData -> Ty.TermBool
asBool (CBool b) = b
asBool t            = error $ unwords [show t, "is not a boolean"]

data CTerm = CTerm { term :: CTermData
                   , udef :: Ty.TermBool
                   }
                   deriving (Show)

instance Bitable CTermData where


nyi :: String -> a
nyi msg = error $ "Not yet implemented: " ++ msg

udefName :: String -> String
udefName s = s ++ "_undef"

newVar :: AST.Type -> String -> Assert CTerm
newVar ty name = do
  u <- Assert.newVar (udefName name) Ty.SortBool
  t <- case ty of
    AST.Bool -> CBool <$> Assert.newVar name Ty.SortBool
    _ | AST.isIntegerType ty ->
      CInt (AST.isSignedInt ty) (AST.numBits ty)
        <$> Assert.newVar name (Ty.SortBv $ AST.numBits ty)
    AST.Double    -> CDouble <$> Assert.newVar name Ty.sortDouble
    AST.Ptr32 _   -> CPtr ty <$> Assert.newVar name (Ty.SortBv 32)
    _             -> nyi $ "newVar for type " ++ show ty
  return $ CTerm t u

-- TODO: shadow memory
cppLoad :: AST.Type -> Bv -> Ty.TermBool -> Mem CTerm
cppLoad ty addr udef' =
  flip CTerm udef' . deserialize ty <$> Mem.memLoad addr (AST.numBits ty)

-- TODO: shadow memory
cppStore :: Bv -> CTerm -> Ty.TermBool -> Mem ()
cppStore addr val guard = Mem.memStore addr (serialize $ term val) (Just guard)

intResize :: Bool -> Int -> Bv -> Bv
intResize fromSign toWidth from =
  let fromWidth = Ty.dynBvWidth from
  in  case compare fromWidth fromWidth of
        LT ->
          (if fromSign then Ty.mkDynBvSext else Ty.mkDynBvUext) toWidth from
        EQ -> from
        GT -> Ty.mkDynBvExtract 0 toWidth from


-- This is not quite right, but it's a reasonable approximation of C++ arithmetic.
-- For an expression l (+) r, 
-- 1. Both l and r undergo **integral promotion** (bool -> int)
-- 2. If either are floating, the other is cast to that floating type
--    * Ptrs not castable.
-- 3. If pointers are allowed:
--    * If there is a pointer, scale the int and do the op
--    * ow error
-- 4. Scale the int, do the op.
cppWrapBinArith
  :: String
  -> (Bv -> Bv -> Bv)
  -> (Ty.TermDouble -> Ty.TermDouble -> Ty.TermDouble)
  -- Undef function, takes sign and Bv term for each argument
  -> Maybe (Bool -> Bv -> Bool -> Bv -> Maybe Ty.TermBool)
  -> Bool -- ^ is plus
  -> Bool -- ^ is minus
  -> Bool -- ^ allow double
  -> Bool -- ^ make width the max of the two (alternative: the left)
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapBinArith name bvF doubleF ubF isAdd isSub allowDouble mergeWidths a b = convert
  (integralPromotion a)
  (integralPromotion b)
 where
  convert a b =
    let
      cannot with = error $ unwords ["Cannot do", name, "with", with]

      cppPtrPlusInt :: AST.Type -> Bv -> Bool -> Bv -> Bv
      cppPtrPlusInt pTy ptr signed int =
        Ty.mkDynBvBinExpr Ty.BvAdd ptr
          $ intResize signed (AST.numBits pTy)
          $ Ty.mkDynBvBinExpr
              Ty.BvMul
              (Mem.bvNum False
                         (Ty.dynBvWidth int)
                         (fromIntegral $ AST.numBits $ AST.pointeeType pTy)
              )
              int

      (t, u) = case (term a, term b) of
        (CDouble d, _) ->
          if allowDouble
          then (CDouble $ doubleF d $ asDouble $ term $ cppCast AST.Double b, Nothing)
          else cannot "a double"
        (_, CDouble d) ->
          if allowDouble
          then (CDouble $ doubleF d $ asDouble $ term $ cppCast AST.Double b, Nothing)
          else cannot "a double"
        (CPtr ty addr, CInt s _ i) -> if isAdd || isSub
          then (CPtr ty $ cppPtrPlusInt ty addr s i, Nothing)
          else cannot "a pointer on the left"
        (CPtr ty addr, CPtr ty' addr') -> if isSub && ty == ty'
          then (CPtr ty (bvF addr addr'), ubF >>= (\f -> f True addr True addr'))
          else cannot "two pointers, or two pointers of different types"
        (CInt s _ i, CPtr ty addr) -> if isAdd
          then (CPtr ty $ cppPtrPlusInt ty addr s i, Nothing)
          else cannot "a pointer on the right"
        -- Ptr diff
        (CInt s w i, CInt s' w' i') ->
          let width = if mergeWidths then max w w' else w
              sign  = max s s'
          in  (CInt sign width
                $ bvF (intResize s width i) (intResize s' width i')
                , ubF >>= (\f -> f s i s' i'))
        (_, _) -> cannot $ unwords [show a, "and", show b]
      pUdef = Ty.BoolNaryExpr Ty.Or (udef a : udef b : Fold.toList u)
    in  CTerm t pUdef

cppBitOr, cppBitXor, cppBitAnd, cppSub, cppMul, cppAdd, cppMin, cppMax, cppDiv, cppRem, cppShl, cppShr:: CTerm -> CTerm -> CTerm
cppAdd = cppWrapBinArith "+" (Ty.mkDynBvBinExpr Ty.BvAdd) (Ty.FpBinExpr Ty.FpAdd) (Just overflow) True False True True
 where overflow s i s' i' = if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSaddo i i' else Nothing
cppSub = cppWrapBinArith "-" (Ty.mkDynBvBinExpr Ty.BvSub) (Ty.FpBinExpr Ty.FpSub) (Just overflow) False True True True
 where overflow s i s' i' = if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSsubo i i' else Nothing
cppMul = cppWrapBinArith "*" (Ty.mkDynBvBinExpr Ty.BvMul) (Ty.FpBinExpr Ty.FpMul) (Just overflow) False False True True
 where overflow s i s' i' = if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSmulo i i' else Nothing
-- TODO: div overflow
cppDiv = cppWrapBinArith "/" (Ty.mkDynBvBinExpr Ty.BvUdiv) (Ty.FpBinExpr Ty.FpDiv) Nothing False False True True
-- TODO: CPP reference says that % requires integral arguments
cppRem = cppWrapBinArith "%" (Ty.mkDynBvBinExpr Ty.BvUrem) (Ty.FpBinExpr Ty.FpRem) Nothing False False False True
cppMin = undefined
cppMax = undefined
cppBitOr = cppWrapBinArith "|" (Ty.mkDynBvBinExpr Ty.BvOr) (const $ const $ error "no fp |") Nothing False False False True
cppBitAnd = cppWrapBinArith "&" (Ty.mkDynBvBinExpr Ty.BvAnd) (const $ const $ error "no fp &") Nothing False False False True
cppBitXor = cppWrapBinArith "^" (Ty.mkDynBvBinExpr Ty.BvXor) (const $ const $ error "no fp ^") Nothing False False False True
-- Not quite right, since we're gonna force these to be equal in size
cppShl = cppWrapBinArith "<<" (Ty.mkDynBvBinExpr Ty.BvShl) (Ty.FpBinExpr Ty.FpAdd) (Just overflow) True True True True
 where
   overflow s i s' i' =
     let baseNonNeg = [Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i) 0) i | s]
         shftNonNeg = [Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i') 0) i' | s']
         shftSmall = [Ty.mkDynBvBinPred Ty.BvSge (Mem.bvNum True (Ty.dynBvWidth i') (fromIntegral $ Ty.dynBvWidth i)) i']
     in  Just $ Ty.BoolNaryExpr Ty.Or $ baseNonNeg ++ shftNonNeg ++ shftSmall
-- Not quite right, since we're gonna force these to be equal in size
cppShr = cppWrapBinArith ">>" (Ty.mkDynBvBinExpr Ty.BvAshr) (Ty.FpBinExpr Ty.FpAdd) (Just overflow) True True True True
 where
   overflow _s i s' i' =
     let shftNonNeg = [Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i') 0) i' | s']
         shftSmall = [Ty.mkDynBvBinPred Ty.BvSge (Mem.bvNum True (Ty.dynBvWidth i') (fromIntegral $ Ty.dynBvWidth i)) i']
     in  Just $ Ty.BoolNaryExpr Ty.Or $ shftNonNeg ++ shftSmall

cppWrapUnArith
  :: String
  -> (Bv -> Bv)
  -> (Ty.TermDouble -> Ty.TermDouble)
  -> CTerm
  -> CTerm
cppWrapUnArith name bvF doubleF a =
  let t = case term $ integralPromotion a of
             CDouble d -> CDouble $ doubleF d
             CInt _ w i -> CInt True w $ bvF i
             _ -> error $ unwords ["Cannot do", name, "on", show a]
  in  CTerm t (udef a)

cppPos, cppNeg, cppBitNot :: CTerm -> CTerm
cppPos = cppWrapUnArith "unary +" id id
cppNeg = cppWrapUnArith "unary -" (Ty.mkDynBvUnExpr Ty.BvNeg) (Ty.FpUnExpr Ty.FpNeg)
cppBitNot = cppWrapUnArith "~" (Ty.mkDynBvUnExpr Ty.BvNot) (Ty.FpUnExpr Ty.FpNeg)

cppWrapBinLogical
  :: String
  -> (Ty.TermBool -> Ty.TermBool -> Ty.TermBool)
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapBinLogical name f a b = case (term $ cppCast AST.Bool a, term $ cppCast AST.Bool b) of
  (CBool a', CBool b') -> CTerm (CBool $ f a' b') (Ty.BoolNaryExpr Ty.Or [udef a , udef b ])
  _ -> error $ unwords ["Cannot do", name, "on", show a, "and", show b]
cppOr, cppAnd :: CTerm -> CTerm -> CTerm
cppOr = cppWrapBinLogical "||" (((.) . (.)) (Ty.BoolNaryExpr Ty.Or) doubleton)
cppAnd = cppWrapBinLogical "&&" (((.) . (.)) (Ty.BoolNaryExpr Ty.And) doubleton)

cppWrapUnLogical
  :: String
  -> (Ty.TermBool -> Ty.TermBool)
  -> CTerm
  -> CTerm
cppWrapUnLogical name f a = case term $ cppCast AST.Bool a of
  CBool a' -> CTerm (CBool $ f a') (udef a)
  _ -> error $ unwords ["Cannot do", name, "on", show a]

cppNot :: CTerm -> CTerm
cppNot = cppWrapUnLogical "!" Ty.Not

-- Promote integral types
-- Do not mess with pointers
integralPromotion :: CTerm -> CTerm
integralPromotion n = case term n of
  CBool{} -> cppCast AST.S32 n
  _       -> n

cppCast :: AST.Type -> CTerm -> CTerm
cppCast toTy node = case term node of
  CBool t -> if AST.isIntegerType toTy
    then
      let width = AST.numBits toTy
          cint  = CInt (AST.isSignedInt toTy) width (boolToBv t width)
      in  CTerm cint (udef node)
    else if AST.isPointer toTy
      then
        let width = AST.numBits toTy
            cptr  = CPtr (AST.pointeeType toTy) (boolToBv t width)
        in  CTerm cptr (udef node)
      else if AST.isDouble toTy
        then CTerm (CDouble $ Ty.DynUbvToFp $ boolToBv t 1) (udef node)
        else if toTy == AST.Bool
          then node
          else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CInt fromS fromW t -> if AST.isIntegerType toTy
    then
      let toW = AST.numBits toTy
          toS = AST.isSignedInt toTy
          t'  = intResize fromS toW t
      in  CTerm (CInt toS toW t') (udef node)
    else if AST.isPointer toTy
      then CTerm (CPtr (AST.pointeeType toTy) (intResize fromS 32 t))
                 (udef node)
      else if AST.isDouble toTy
        then CTerm
          (CDouble $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
          (udef node)
        else if toTy == AST.Bool
          then CTerm (CBool $ Ty.Not $ Ty.Eq (Mem.bvNum False fromW 0) t)
                     (udef node)
          else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CDouble t -> if AST.isIntegerType toTy
    then CTerm
      ( CInt (AST.isSignedInt toTy) (AST.numBits toTy)
      $ Ty.RoundFpToDynBv (AST.numBits toTy) True t
      )
      (udef node)
    else if toTy == AST.Bool
      then CTerm (CBool $ Ty.FpBinPred Ty.FpEq (Ty.Fp64Lit 0.0) t) (udef node)
      else if AST.isDouble toTy
        then node
        else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CPtr _ t -> if AST.isIntegerType toTy
    then cppCast toTy $ CTerm (CInt False (Ty.dynBvWidth t) t) (udef node)
    else if AST.isPointer toTy
      -- TODO: Not quite right: widths
      then node
      else error $ unwords ["Bad cast from", show t, "to", show toTy]
 where
  boolToBv :: Ty.TermBool -> Int -> Bv
  boolToBv b w = Ty.Ite b (Mem.bvNum False w 1) (Mem.bvNum False w 0)

cppCond :: CTerm
        -> CTerm
        -> CTerm
        -> CTerm
cppCond cond t f = let
  condB = asBool $ term $ cppCast AST.Bool cond
  result = case (term t, term f) of
    (CBool tB, CBool fB) -> CBool $ Ty.Ite condB tB fB
    (CDouble tB, CDouble fB) -> CDouble $ Ty.Ite condB tB fB
    (CPtr tTy tB, CPtr fTy fB) | tTy == fTy -> CPtr tTy $ Ty.Ite condB tB fB
    (CInt s w i, CInt s' w' i') ->
      let sign = s && s'
          width = max w w'
      in  CInt sign width $ Ty.Ite condB (intResize s width i) (intResize s' width i')
    _ -> error $ unwords ["Cannot construct conditional with", show t, "and", show f]
  in CTerm result (Ty.BoolNaryExpr Ty.Or [udef cond, udef t, udef f])


cppAssign :: CTerm
          -> CTerm
          -> Assert CTerm
cppAssign l r = let
  r' = cppCast (ctermDataTy $ term l) r
  op = case (term l, term r') of
    (CBool lB, CBool rB) -> Assert.assign lB rB
    (CDouble lB, CDouble rB) -> Assert.assign lB rB
    (CInt _ _ lB, CInt _ _ rB) -> Assert.assign lB rB
    (CPtr lTy lB, CPtr rTy rB) | lTy == rTy -> Assert.assign lB rB
    _ -> error "Invalid cppAssign terms, post-cast"
  in op *> pure r'

doubleton :: a -> a -> [a]
doubleton x y = [x, y]

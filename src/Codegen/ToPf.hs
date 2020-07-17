{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Codegen.ToPf
  ( toPf
  )
where

import           IR.TySmt
import           Control.Monad.State.Strict
import           Control.Monad                  ( )
import           GHC.TypeNats
import           Codegen.Circom.CompTypes.LowDeg
import qualified Codegen.ShowMap               as SMap
import           Codegen.ShowMap                ( ShowMap )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Typeable                  ( cast )

type PfVar = String

type LSig n = LC PfVar (Prime n)

data ToPfState n = ToPfState { qeqs :: [QEQ PfVar (Prime n)]
                             , bools :: ShowMap TermBool (LSig n)
                             , ints :: ShowMap TermDynBv (BvEntry n)
                             , next :: Int
                             }

newtype ToPf n a = ToPf (StateT (ToPfState n) IO a)
    deriving (Functor, Applicative, Monad, MonadState (ToPfState n), MonadIO)

emptyState :: ToPfState n
emptyState =
  ToPfState { qeqs = [], bools = SMap.empty, ints = SMap.empty, next = 0 }

-- # Constraints

enforce :: KnownNat n => QEQ PfVar (Prime n) -> ToPf n ()
enforce qeq = modify (\s -> s { qeqs = qeq : qeqs s })

enforceTrue :: KnownNat n => LSig n -> ToPf n ()
enforceTrue s = enforce (lcZero, lcZero, lcSub s lcOne)

-- # Variables

nextVar :: ToPf n PfVar
nextVar = do
  i <- gets next
  modify (\s -> s { next = 1 + next s })
  return $ "v" ++ show i

-- # Bit constraints and storage

saveBool :: KnownNat n => TermBool -> LSig n -> ToPf n ()
saveBool b x = modify (\s -> s { bools = SMap.insert b x $ bools s })

lookupBool :: KnownNat n => TermBool -> ToPf n (Maybe (LSig n))
lookupBool b = gets (SMap.lookup b . bools)

requireBit :: KnownNat n => LSig n -> ToPf n ()
requireBit v = enforce (v, lcShift (negate 1) v, lcZero)

lcOne :: KnownNat n => LSig n
lcOne = lcShift (toP 1) lcZero

lcNeg :: KnownNat n => LSig n -> LSig n
lcNeg = lcScale (toP $ negate 1)

lcSub :: KnownNat n => LSig n -> LSig n -> LSig n
lcSub x y = lcAdd x $ lcNeg y

lcNot :: KnownNat n => LSig n -> LSig n
lcNot = lcSub lcOne

boolToPf :: KnownNat n => TermBool -> ToPf n (LSig n)
boolToPf term = do
  entry <- lookupBool term
  case entry of
    Just s  -> return s
    Nothing -> do
      s <- boolToPfUncached term
      saveBool term s
      return s
 where
  -- Uncached
  boolToPfUncached :: KnownNat n => TermBool -> ToPf n (LSig n)
  boolToPfUncached t = case t of
    Eq a b -> do
      a' <- boolToPf $ fromMaybe (error "not bool") $ cast a
      b' <- boolToPf $ fromMaybe (error "not bool") $ cast b
      binEq a' b'
    BoolLit b  -> return $ lcShift (toP $ fromIntegral $ fromEnum b) lcZero
    Not     a  -> lcNot <$> boolToPf a
    Var name _ -> do
      let v = lcSig name
      requireBit v
      return v
    BoolNaryExpr o xs -> do
      xs' <- traverse boolToPf xs
      case xs' of
        []  -> pure $ lcShift (toP $ fromIntegral $ fromEnum $ opId o) lcZero
        [a] -> pure a
        _   -> case o of
          Or  -> naryOr xs'
          And -> naryAnd xs'
          Xor -> naryXor xs'
    BoolBinExpr Implies a b -> do
      a' <- boolToPf a
      b' <- boolToPf b
      impl a' b'
    Ite c t_ f -> do
      c' <- boolToPf c
      t' <- boolToPf t_
      f' <- boolToPf f
      v  <- lcSig <$> nextVar
      enforce (c', lcSub v t', lcZero)
      enforce (lcNot c', lcSub v f', lcZero)
      return v
    _ -> error $ unlines ["The term", show t, "is not supported in boolToPf"]
  opId :: BoolNaryOp -> Bool
  opId o = case o of
    And -> True
    Or  -> False
    Xor -> False
  -- TODO: There is a better implementation for binary (even ternary?) AND/OR.
  -- It is based on AND as multiplication
  naryAnd :: KnownNat n => [LSig n] -> ToPf n (LSig n)
  naryAnd xs = if length xs <= 3
    then foldM binAnd (head xs) (tail xs)
    else lcNot <$> naryOr (map lcNot xs)

  binAnd :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
  binAnd a b = do
    v <- lcSig <$> nextVar
    enforce (a, b, v)
    return v
  naryOr :: KnownNat n => [LSig n] -> ToPf n (LSig n)
  naryOr xs = if length xs <= 3
    then lcNot <$> naryAnd (map lcNot xs)
    else
      let s = foldl1 lcAdd xs
      in  do
            or' <- lcSig <$> nextVar
            enforce (s, lcSub lcOne or', lcZero)
            requireBit or'
            inv <- lcSig <$> nextVar
            enforce (lcSub (lcAdd lcOne s) or', inv, lcOne)
            return or'
  impl :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
  impl a b = do
    v <- lcSig <$> nextVar
    enforce (a, lcNot b, lcNot v)
    return v
  binEq :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
  binEq a b = do
    v <- lcSig <$> nextVar
    requireBit v
    enforce (lcSub a b, v, lcZero)
    inv <- lcSig <$> nextVar
    enforce (lcAdd (lcSub a b) v, inv, lcOne)
    return v
  binXor :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
  binXor a b = lcNot <$> binEq a b
  naryXor :: KnownNat n => [LSig n] -> ToPf n (LSig n)
  naryXor xs = foldM binXor (head xs) (tail xs)

-- # Arith constraints and storage

-- The integer entry holds a (signal, width) pair
data BvEntry n = BvEntry { int :: Maybe (LSig n, Int)
                         , bits :: Maybe [LSig n]
                         }

bvEntryEmpty :: BvEntry n
bvEntryEmpty = BvEntry { int = Nothing, bits = Nothing }


-- Initialize an empty entry
initIntEntry :: TermDynBv -> ToPf n ()
initIntEntry t =
  modify $ \s -> s { ints = SMap.insertWith (const id) t bvEntryEmpty $ ints s }


saveInt :: TermDynBv -> (LSig n, Int) -> ToPf n ()
saveInt term sig = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { int = Just sig }) term $ ints s })

saveIntBits :: TermDynBv -> [LSig n] -> ToPf n ()
saveIntBits term bits_ = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { bits = Just bits_ }) term $ ints s })

getInt :: TermDynBv -> ToPf n (Maybe (LSig n, Int))
getInt term = do
  e <- gets (SMap.lookup term . ints)
  case e >>= int of
    Just i  -> return $ Just i
    Nothing -> case e >>= bits of
      Just bs -> do
        i <- deBitify bs
        saveInt term (i, length bs)
        return $ Just (i, length bs)
      Nothing -> return Nothing

getIntBits :: KnownNat n => TermDynBv -> ToPf n (Maybe [LSig n])
getIntBits term = do
  e <- gets (SMap.lookup term . ints)
  case e >>= bits of
    Just bs -> return $ Just bs
    Nothing -> case e >>= int of
      Just (i, width) -> do
        bs <- bitify i width
        saveIntBits term bs
        return $ Just bs
      Nothing -> return Nothing

bitify :: KnownNat n => LSig n -> Int -> ToPf n [LSig n]
bitify i width = do
  sigs <- replicateM width (lcSig <$> nextVar)
  forM_ sigs requireBit
  let sum' = foldr1 lcAdd $ zipWith lcScale
        (map (toP . (2 ^)) [0 ..])
        sigs
  enforce (lcZero, lcZero, lcSub sum' i)
  return sigs

deBitify :: [LSig n] -> ToPf n (LSig n)
deBitify = undefined

-- # Top Level

enforceAsPf :: KnownNat n => TermBool -> ToPf n ()
enforceAsPf b = boolToPf b >>= enforceTrue

runToPf :: KnownNat n => ToPf n a -> ToPfState n -> IO (a, ToPfState n)
runToPf (ToPf f) = runStateT f

toPf :: KnownNat n => [TermBool] -> IO [QEQ PfVar (Prime n)]
toPf bs = qeqs . snd <$> runToPf (forM_ bs enforceAsPf) emptyState

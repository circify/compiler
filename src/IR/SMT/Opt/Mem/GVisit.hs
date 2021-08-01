{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Util
Description : General SMT Mem traversals

= ArrReplace Pass

For visiting and optionally replacing memory interactions.

For Field-keyed arrays.
-}
module IR.SMT.Opt.Mem.GVisit
  ( ArraySizes
  , computeSizes
  , TArr
  , ArrReplacePass(..)
  , runArrReplacePass
  , defaultArrReplacePass
  )
where
import           Control.Monad.State.Strict     ( void
                                                , unless
                                                , when
                                                , forM_
                                                )
import           IR.SMT.TySmt
import           IR.SMT.TySmt.Alg               ( mapTermM )
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Log                       ( logIf
                                                , Log
                                                , MonadLog
                                                )
import qualified Util.Progress                 as P
import           Util.Progress                  ( Progress )
import           Data.Typeable                  ( cast
                                                , Typeable
                                                , eqT
                                                , (:~:)(..)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           GHC.TypeNats                   ( KnownNat )


type ArrSort n v = ArraySort (PfSort n) v
type TArr n v = Term (ArrSort n v)
type ArraySizes n v = ShowMap (TArr n v) Int

-- replacing field-indexed arrays
data ArrReplacePass m n v = ArrReplacePass
  { -- Each function gets the original and new version of each child.
    -- Original versions first. They may be useful for interacting with a
    -- preprocessing analysis.

    -- No previous sort, since it is unchanged
    visitConstArray :: Int -> (Term v) -> Sort -> (Term v) -> m ()
  , visitEq         :: (TArr n v) -> (TArr n v) -> (TArr n v) -> (TArr n v) -> m (Maybe TermBool)
  , visitIte :: TermBool -> (TArr n v) -> (TArr n v) -> TermBool -> (TArr n v) -> (TArr n v) -> m ()
  , visitStore      :: (TArr n v) -> (TermPf n) -> (Term v) -> (TArr n v) -> (TermPf n) -> (Term v) -> m ()
  , visitSelect     :: (TArr n v) -> (TermPf n) -> (TArr n v) -> (TermPf n) -> m (Maybe (Term v))
    -- No previous children, as they do not change.
  , visitVar        :: String -> Sort -> m ()
  , preVisitConstArray :: Int -> (Term v) -> Sort -> m ()
  , preVisitEq         :: (TArr n v) -> (TArr n v) -> m ()
  , preVisitIte :: TermBool -> (TArr n v) -> (TArr n v) -> m ()
  , preVisitStore      :: (TArr n v) -> (TermPf n) -> (Term v) -> m ()
  , preVisitSelect     :: (TArr n v) -> (TermPf n) -> m ()
  }

defaultArrReplacePass :: forall m n v . Monad m => ArrReplacePass m n v
defaultArrReplacePass = ArrReplacePass
  { visitConstArray    = \_ _ _ _ -> return ()
  , visitEq            = \_ _ _ _ -> return Nothing
  , visitIte           = \_ _ _ _ _ _ -> return ()
  , visitVar           = \_ _ -> return ()
  , visitStore         = \_ _ _ _ _ _ -> return ()
  , visitSelect        = \_ _ _ _ -> return Nothing
  , preVisitConstArray = \_ _ _ -> return ()
  , preVisitEq         = \_ _ -> return ()
  , preVisitIte        = \_ _ _ -> return ()
  , preVisitStore      = \_ _ _ -> return ()
  , preVisitSelect     = \_ _ -> return ()
  }

runArrReplacePass
  :: forall m n v
   . (MonadLog m, KnownNat n, SortClass v)
  => ArrReplacePass m n v
  -> TermBool
  -> m TermBool
runArrReplacePass pass = mapTermM visit
 where
  -- Force cast
  fCast :: (Typeable a, Typeable b) => a -> b
  fCast = fromMaybe (error "Bad cast") . cast

  visit :: forall s . SortClass s => Term s -> m (Maybe (Term s))
  visit t = case eqT @(Term s) @(TArr n v) of
    Just Refl -> case t of
      ConstArray l sort value -> do
        preVisitConstArray pass l value sort
        value' <- rec value
        logIf "mem::replace::pass" $ "visitConstArray: " ++ show t
        visitConstArray pass l value sort value'
        return $ Just $ ConstArray l sort value'
      Store array idx value -> do
        preVisitStore pass array idx value
        array' <- rec array
        idx'   <- rec idx
        value' <- rec value
        logIf "mem::replace::pass" $ "visitStore: " ++ show t
        visitStore pass array idx value array' idx' value'
        return $ Just $ Store array' idx' value'
      Ite c a b -> do
        preVisitIte pass c a b
        c' <- rec c
        a' <- rec a
        b' <- rec b
        logIf "mem::replace::pass" $ "visitIte: " ++ show t
        visitIte pass c a b c' a' b'
        return $ Just $ Ite c' a' b'
      Var name sort -> do
        logIf "mem::replace::pass" $ "visitVar: " ++ show t
        visitVar pass name sort
        return $ Just t
      Exists{} -> error "nyi: existential memories"
      Let{}    -> error "nyi: let bindings for memories"
      Select{} -> error "nyi: selecting a member from an array"
    Nothing -> case t of
      Select array idx -> do
        --preVisitSelect pass (fCast array) (fCast idx)
        forM_ (cast (array, idx)) $ \(a0', a1') -> do
            logIf "mem::replace::pass" $ "preVisitSelect: " ++ show t
            preVisitSelect pass a0' a1'
        array'' <- rec array
        idx''   <- rec idx
        Just <$> case cast (array'', idx'') of
          Just (array', idx') -> do
            logIf "mem::replace::pass" $ "visitSelect: " ++ show t
            mReplaced <- visitSelect pass (fCast array) (fCast idx) array' idx'
            return $ fCast $ fromMaybe (Select array' idx') mReplaced
          Nothing -> return $ Select array'' idx''
      Eq a0 a1 -> do
        forM_ (cast (a0, a1)) $ \(a0', a1') -> do
            logIf "mem::replace::pass" $ "preVisitEq: " ++ show t
            preVisitEq pass a0' a1'
        a0' <- rec a0
        a1' <- rec a1
        Just <$> case cast (a0', a1') of
          Just (a0'', a1'') -> do
            logIf "mem::replace::pass" $ "visitEq: " ++ show t
            mReplaced <- visitEq pass (fCast a0) (fCast a1) a0'' a1''
            return $ fromMaybe (mkEq a0'' a1'') mReplaced
          _ -> return $ Eq a0' a1'
      _ -> return Nothing
   where
    rec :: SortClass s2 => Term s2 -> m (Term s2)
    rec = mapTermM visit

-- | Propagate static array size through a formula.
computeSizes
  :: forall n v
   . (KnownNat n, SortClass v)
  => [TermBool]
  -> Log (ArraySizes n v)
computeSizes ts = P.runToFixPoint pass SMap.empty
 where
  equateSizes :: (TArr n v) -> (TArr n v) -> Progress (ArraySizes n v) ()
  equateSizes a b = do
    mAS <- P.gets $ SMap.lookup a
    mBS <- P.gets $ SMap.lookup b
    case (mAS, mBS) of
      (Just aS, Just bS) -> unless (aS == bS) $ error $ unlines
        [ "Unequal array sizes:"
        , "size " ++ show aS ++ ": " ++ show a
        , "size " ++ show bS ++ ": " ++ show b
        ]
      (Just aS, Nothing) -> setSize b aS
      (Nothing, Just bS) -> setSize a bS
      (Nothing, Nothing) -> return ()

  setSize :: (TArr n v) -> Int -> Progress (ArraySizes n v) ()
  setSize t s = do
    knownAlready <- P.gets $ SMap.member t
    when (not knownAlready) $ do
      P.setProgress
      P.modify (SMap.insert t s)
      logIf "array::elim::sizes" $ unwords ["Size", show s, show t]

  onePass = defaultArrReplacePass
    { visitStore         = \a i v _ _ _ -> equateSizes (Store a i v) a
    , preVisitStore      = \a i v -> equateSizes (Store a i v) a
    , visitIte           = \c t f _ _ _ -> do
                             equateSizes (Ite c t f) t
                             equateSizes t           f
                             equateSizes (Ite c t f) f
    , preVisitIte        = \c t f -> do
                             equateSizes (Ite c t f) t
                             equateSizes t           f
                             equateSizes (Ite c t f) f
    , visitEq            = \a b _ _ -> equateSizes a b >> return Nothing
    , preVisitEq         = \a b -> equateSizes a b
    , visitConstArray    = \l v s _ -> setSize (ConstArray l s v) l
    , preVisitConstArray = \l v s -> setSize (ConstArray l s v) l
    }

  pass :: Progress (ArraySizes n v) ()
  pass = do
    logIf "array::elim::sizes" "Start sizes pass"
    -- We traverse forwards and then backwards, since assertions fall in
    -- assignment order, and natural propagation can go either way.
    void $ mapM (runArrReplacePass onePass) (ts ++ reverse ts)

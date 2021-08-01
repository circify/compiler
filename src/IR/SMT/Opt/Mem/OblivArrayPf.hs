{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : OblivArrayPf
Description : Elimination of array terms which are accessed only at constant indices.

= Overview

This module attempts to identify *oblivious* arrays: those that are only
accessed at constant indices[1]. These arrays can be replaced with normal terms.

It operates in three passes: (1) determine which arrays are oblivious (2)
compute the size of the array back all array terms and (3) replace oblivious
arrays with (haskell) lists of terms.

== Pass 1: Identifying oblivious arrays

We maintain a set of non-oblivious arrays, initially empty. We traverse the
whole SMT constraint system, performing the following inferences:

* If @a[i]@ for non-constant @i@, then @a@ is not oblivious
* If @a[i\v]@ for non-constant @i@, then neither @a[i\v]@ nor @a@ are oblivious
* If @a[i\v]@, then @a[i\v]@ and @a@ are equi-oblivious
* If @ite(c,a,b)@, then @ite(c,a,b)@, @a@, and @b@ are equi-oblivious
* If @a=b@, then @a@ and @b@ are equi-oblivious

This procedure is iterated to fixpoint.

== Pass 2: Computing array sizes

This is done similar to the above, propagating array sizes instead of
non-obliviousness. If a conflict is found, throw an error.

== Pass 3: Replacing oblivious arrays with term lists.

In this pass, the goal is to

* map array terms to haskell lists of value terms
* map array selections to specific value terms

The pass maintains:

* a map from array terms to lists of values

It then does a bottom-up formula traversal, performing the following
transformations:

* oblivious array variables are mapped to a list of (derivative)
  variables
* oblivious constant arrays are mapped to a list that replicates the
  constant
* accesses to oblivious arrays are mapped to the appropriate term from the
  value list of the array
* stores to oblivious arrays are mapped to updated value lists
* equalities between oblivious arrays are mapped to conjunctions of equalities

[1]: Our use of "oblivious" is inspired by *oblivious turing
machines* <https://en.wikipedia.org/wiki/Turing_machine_equivalents#Oblivious_Turing_machines>
which access their tape in a data-indepedant way.

-}

module IR.SMT.Opt.Mem.OblivArrayPf
  ( elimOblivArrays
  )
where

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import qualified Data.Set                      as Set
import           GHC.TypeNats                   ( KnownNat )
import           IR.SMT.TySmt
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )
import           IR.SMT.Opt.Mem.GVisit
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Control                   ( whenM )
import           Util.Cfg                       ( MonadCfg(..) )
import           Util.Log
import qualified Util.Progress                 as P
import           Util.Progress                  ( Progress )
import           Util.Show

type ArraySet n v = ShowMap (TArr n v) ()

-- | Given a list of assertions, build a set of array terms which are *not*
-- oblivious.
findNonObliviousArrays
  :: forall n v. (KnownNat n, SortClass v) => [TermBool] -> Log (ArraySet n v)
findNonObliviousArrays ts = P.runToFixPoint pass SMap.empty
 where
  -- |
  --
  -- Mark this array non-oblivious, and if it wasn't already so marked set the
  -- progress flag.
  markNotOblivious :: (TArr n v) -> Progress (ArraySet n v) ()
  markNotOblivious a = do
    alreadyMarked <- gets (SMap.member a . fst)
    unless alreadyMarked $ do
      logIf "array::elim::mark" $ "Marking: " ++ show a
      P.modify $ SMap.insert a ()
      P.setProgress

  isPfLiteral :: TermPf n -> Bool
  isPfLiteral t = case t of
    IntToPf (IntLit _) -> True
    _                  -> False

  -- | Assert that the non-obliviousness of a implies that of b.
  implicate :: (TArr n v) -> (TArr n v) -> Progress (ArraySet n v) ()
  implicate a b = whenM (gets (SMap.member a . fst)) $ markNotOblivious b

  -- | Assert equality of obliviousness
  biImplicate a b = implicate a b >> implicate b a

  onePass = defaultArrReplacePass
    { visitSelect = \_ _ a i ->
      unless (isPfLiteral i) (markNotOblivious a) >> return Nothing
    , visitStore  = \a i v _ _ _ -> if isPfLiteral i
                      then biImplicate a (Store a i v)
                      else markNotOblivious a >> markNotOblivious (Store a i v)
    , visitEq     = \a b _ _ -> biImplicate a b >> return Nothing
    , visitIte    = \c t f _ _ _ -> do
                      biImplicate f (Ite c t f)
                      biImplicate t (Ite c t f)
                      biImplicate t f
    }

  pass :: Progress (ArraySet n v) ()
  pass = do
    logIf "array::elim::mark" "Start mark pass"
    void $ mapM (runArrReplacePass onePass) ts


type TermListMap n v = ShowMap (TArr n v) [Term v]

newtype Obliv n v a = Obliv (StateT (TermListMap n v) Assert a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState (TermListMap n v), OA.MonadAssert)

modList :: Int -> a -> [a] -> [a]
modList _ _ []      = error "oob modList"
modList 0 v (_ : t) = v : t
modList i v (h : t) = h : modList (i - 1) v t


replaceObliviousArrays
  :: forall n v
  .  (KnownNat n, SortClass v)
  => (Set.Set String)
  -> (ArraySizes n v)
  -> (ArraySet n v)
  -> Assert ()
replaceObliviousArrays noElim arraySizes nonOblivious = evalStateT pass SMap.empty
 where
  asConstInt :: TermPf n -> Maybe Int
  asConstInt t = case t of
    IntToPf (IntLit i) -> Just (fromIntegral i)
    _                  -> Nothing

  isOblivious :: (TArr n v) -> Bool
  isOblivious t = not $ SMap.member t nonOblivious

  size :: (TArr n v) -> Int
  size t =
    fromMaybe (error $ "No size for " ++ show t) $ SMap.lookup t arraySizes

  getTerms :: (TArr n v) -> Obliv n v [Term v]
  getTerms array = gets
    (fromMaybe (error $ "No value list for " ++ show array) . SMap.lookup array)

  atIndex :: Int -> [a] -> a
  atIndex i =
    fromMaybe (error $ "No value at index " ++ show i) . listToMaybe . drop i

  valueSort :: Sort -> Sort
  valueSort s = case s of
    SortArray _ v -> v
    _             -> error $ "Sort " ++ show s ++ " is not an array sort"

  idxWidth :: Sort -> Int
  idxWidth s = case s of
    SortArray (SortBv i) _ -> i
    _ -> error $ "Sort " ++ show s ++ " is not an array(Bv,_) sort"

  store :: (TArr n v) -> [Term v] -> Obliv n v ()
  store t l = do
    -- We truncate the list, because ConstArrays have an infinite list
    logIf "array::elim::replace" $ "Replace: " ++ show t ++ " -> " ++ show
      (take 10 l)
    modify $ SMap.insert t l

  visitors = (defaultArrReplacePass :: ArrReplacePass (Obliv n v) n v)
    { visitConstArray = \l v sort v' ->
                          let c = ConstArray l sort v
                          in  when (isOblivious c) $ store c $ repeat v'
    , visitStore      = \a i v a' i' v' -> case asConstInt i' of
                          Just ci | isOblivious (Store a i v) -> do
                            logIf "array::elim::store" $ "Store: " ++ show
                              (Store a i v)
                            l' <- modList ci v' <$> getTerms a'
                            logIf "array::elim::store" $ " => " ++ show l'
                            store (Store a' i' v') l'
                          _ -> return ()
    , visitIte        = \c t f c' t' f' ->
                          when (isOblivious (Ite c t f))
                            $ liftM2 (zipWith (Ite c')) (getTerms t') (getTerms f')
                            >>= store (Ite c' t' f')
    , visitVar        = \name sort -> do
      let var = Var name sort
      when (isOblivious var && Set.member name noElim) $ do
        let varName i = name ++ "_" ++ show i
        let w = idxWidth sort
        logIf "array::elim::var" $ "Variable: " ++ name ++ " size " ++ show (size var)
        ts :: [Term v] <- forM [0 .. (size var - 1)] $ \i -> do
          let s = mkSelect (Var name sort) $ DynBvLit $ Bv.bitVec w i
          OA.newVar (varName i) (valueSort sort) s
        store var ts
    , visitSelect     = \a _ a' i' -> case asConstInt i' of
                          Just ci | isOblivious a ->
                            Just . atIndex ci <$> getTerms a'
                          _ -> return Nothing
    , visitEq         = \a _ a' b' -> if isOblivious a
                          then
                            case (a', b') of
                              (Var name _, right) ->
                                if not (Set.member name noElim)
                                  then do
                                    l' <- getTerms right
                                    store a' l'
                                    return $ Just $ BoolLit True
                                  else
                                    Just . BoolNaryExpr And <$> liftM2 (zipWith mkEq)
                                                                            (getTerms a')
                                                                            (getTerms b')
                              _ -> 
                                    Just . BoolNaryExpr And <$> liftM2 (zipWith mkEq)
                                                                            (getTerms a')
                                                                            (getTerms b')
                          else return Nothing
    }

  Obliv pass = OA.modifyAssertionsWith $ runArrReplacePass visitors


elimOblivArrays :: forall n v.(KnownNat n, SortClass v) => Assert ()
elimOblivArrays = do
  -- TODO: push Asssert deeper?
  --sizes        <- gets (OA._sizes)
  noElim <- gets OA._public
  as           <- gets OA.listAssertions
  nonOblivious <- liftLog $ findNonObliviousArrays @n @v as
  logIf "array::elim::sizes" $ "start sizes"
  sizes' <- liftLog $ computeSizes as
  logIf "array::elim::sizes" $ "end sizes"
  logIf "array::elim::sizes" $ "sizes: " ++ pShow sizes'
  replaceObliviousArrays noElim sizes' nonOblivious

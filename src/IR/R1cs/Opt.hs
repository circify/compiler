{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.R1cs.Opt
  ( opt
  )
where

import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           Codegen.Circom.Signal
import           IR.R1cs
import           Codegen.Circom.CompTypes.LowDeg
                                                ( QEQ
                                                , LC
                                                , lcZero
                                                , lcAdd
                                                , lcScale
                                                )
import           Control.Applicative
import           GHC.TypeLits                   ( KnownNat )
import           Data.Bifunctor
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Foldable                 as Fold
import qualified Data.List                     as List
import qualified Data.Sequence                 as Seq
import           Debug.Trace

-- TODO: this would all be a lot fast if the constraints used IntMaps...

-- If this QEQ implies that some signal is an affine function of another,
-- return that.
-- `protected` is a set of variables we should not eliminate
asLinearSub
  :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Maybe (Int, LC Int k)
asLinearSub protected (a, b, (m, c)) = if a == lcZero && b == lcZero
  then
    -- TODO: First or last?
    let here = IntSet.fromDistinctAscList $ Map.keys m
        disjoint = here IntSet.\\ protected
    in  case IntSet.toList disjoint of
          [] -> Nothing
          k : _ ->
            let v  = m Map.! k
                m' = Map.delete k m
            in  Just (k, lcScale (negate $ recip v) (m', c))
  else Nothing

type Subs n = Map.Map Int (LC Int (Prime n))
type Assertion n = QEQ Int (Prime n)

data SubState n = SubState { subs :: !(Subs n)
                           , pub :: !IntSet.IntSet
                           }

emptySub :: SubState n
emptySub = SubState { subs = Map.empty, pub = IntSet.empty }

newtype Sub n a = Sub (StateT (SubState n) Identity a)
    deriving (Functor, Applicative, Monad, MonadState (SubState n))


accumulateSubs :: (KnownNat n, Show s) => R1CS s n -> Sub n ()
accumulateSubs r1cs = do
  modify $ \s -> s { pub = publicInputs r1cs }
  forM_ (constraints r1cs) process
 where
  process :: KnownNat n => QEQ Int (Prime n) -> Sub n ()
  process qeq' = do
    qeq       <- applyStoredSubs qeq'
    protected <- gets pub
    case asLinearSub protected qeq of
      Just (v, t) -> addSub v t
      Nothing     -> return ()

  addSub :: KnownNat n => Int -> LC Int (Prime n) -> Sub n ()
  addSub v t = modify $ \s -> s
    { subs = Map.insert v t $ Map.map (subLcsInLc (Map.singleton v t)) $ subs s
    }

  applyStoredSubs :: KnownNat n => Assertion n -> Sub n (Assertion n)
  applyStoredSubs qeq = flip subLcsInQeq qeq <$> gets subs

applyLinearSubs :: KnownNat n => Subs n -> R1CS s n -> R1CS s n
applyLinearSubs subs r1cs =
  let removed = IntSet.fromAscList $ Map.keys subs
      qeqZero = (lcZero, lcZero, lcZero)
  in  r1cs
        { constraints = Seq.filter (/= qeqZero) $ subLcsInQeq subs <$> constraints r1cs
        , numSigs = numSigs r1cs IntMap.\\ IntMap.fromDistinctAscList
                      (map (, SigLocal ("", [])) $ IntSet.toAscList removed)
        , sigNums = Map.filter (not . (`IntSet.member` removed)) $ sigNums r1cs
        }

subLcsInLc
  :: forall s k
   . (Ord s, GaloisField k)
  => Map.Map s (LC s k)
  -> LC s k
  -> LC s k
subLcsInLc subs (m, c) =
  let additional :: [LC s k] =
          Fold.toList $ Map.intersectionWith lcScale m subs
      unmodified = (m Map.\\ subs, c)
  in  Fold.foldl' lcAdd unmodified additional

subLcsInQeq
  :: (Ord s, GaloisField k) => Map.Map s (LC s k) -> QEQ s k -> QEQ s k
subLcsInQeq subs (a, b, c) =
  (subLcsInLc subs a, subLcsInLc subs b, subLcsInLc subs c)

reduceLinearities :: (KnownNat n, Show s) => R1CS s n -> R1CS s n
reduceLinearities r1cs =
  let Sub s = accumulateSubs r1cs
      m     = subs $ execState s emptySub
  in  applyLinearSubs m r1cs

opt :: (KnownNat n, Show s) => R1CS s n -> R1CS s n
opt = compactifySigNums . removeDeadSignals . reduceLinearities


-- Remove signals not involved in constraints
removeDeadSignals :: R1CS s n -> R1CS s n
removeDeadSignals r1cs =
  let liveSigs = liveSignalIntsR1cs r1cs `IntSet.union` publicInputs r1cs
  in  r1cs
        { numSigs    = IntMap.filterWithKey (\k v -> IntSet.member k liveSigs)
                         $ numSigs r1cs
        , sigNums    = Map.filter (`IntSet.member` liveSigs) $ sigNums r1cs
        , nextSigNum = 2 + IntSet.size liveSigs
        }
 where
  liveSignalIntsLc (m, c) = Map.keys m
  liveSignalIntsQeq (a, b, c) =
    IntSet.fromList (concatMap liveSignalIntsLc [a, b, c])
  liveSignalIntsR1cs =
    Fold.foldr IntSet.union IntSet.empty . fmap liveSignalIntsQeq . constraints

-- Given a set of constraints, ensures that the signal numbers are in the range
-- [2..(1+n)], where n is the number of signals
compactifySigNums :: R1CS s n -> R1CS s n
compactifySigNums r1cs =
  let usedNums = IntMap.keys $ numSigs r1cs
      numMap   = IntMap.fromDistinctAscList $ zip usedNums [2 ..]
      remap s i =
          Maybe.fromMaybe
              (  error
              $  "Could not find signal number "
              ++ show i
              ++ " when remapping signal numbers.\n\nContext: "
              ++ s
              )
            $         numMap
            IntMap.!? i
  in  R1CS
        { sigNums      = Map.map (remap "sigNums") $ sigNums r1cs
        , numSigs = IntMap.mapKeysMonotonic (remap "numSigs") $ numSigs r1cs
        , constraints  = sigMapQeq (remap "constraints") <$> constraints r1cs
        , publicInputs = IntSet.map (remap "publicInputs") $ publicInputs r1cs
        , nextSigNum   = 2 + IntMap.size numMap
        }

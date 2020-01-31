{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Codegen.Circom.Term ( lcZero
                           , Term(..)
                           , LTerm(..)
                           , Signal(..)
                           , Constraint
                           , LC
                           , mapSignalsInTerm
                           , termSignals
                           ) where

import qualified Codegen.Circom.Constraints as CS
import           Codegen.Circom.Constraints (Constraints, Constraint, LC, Signal)
import           Data.Field.Galois          (PrimeField)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.Set                   (Set)

data Term k = Sig Signal
            | Linear (LC k)                                     -- A linear combination
            | Quadratic (LC k) (LC k) (LC k)                    -- A * B + C for LC's A, B, C
            | Scalar k                                          -- a gen-time constant
            | Array [Term k]                                    -- An array of terms
            | Struct (Map.Map String (Term k)) (Constraints k)  -- A structure, environment and constraints
            | Other                                             -- A non-gen-time constant that is none of the above.
            deriving (Show,Ord,Eq)

-- An evaluated l-value
data LTerm = LTermIdent String
           | LTermPin LTerm String
           | LTermIdx LTerm Int
           deriving (Show,Ord,Eq)

instance PrimeField k => Num (Term k) where
  s + t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Other, _) -> Other
    (Sig s, r) -> linearizeSig s + r
    (s, Sig r) -> s + linearizeSig r
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> r + l
  s * t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Other, _) -> Other
    (Sig s, r) -> linearizeSig s * r
    (s, Sig r) -> s * linearizeSig r
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum s = case s of
    Array {}     -> error $ "Cannot get sign of array term " ++ show s
    Struct {}    -> error $ "Cannot get sign of struct term " ++ show s
    Other        -> Scalar 1
    Sig {}       -> Scalar 1
    Linear {}    -> Scalar 1
    Quadratic {} -> Scalar 1
    Scalar n     -> Scalar $ signum n
  abs s = case s of
    Array a        -> Scalar $ fromIntegral $ length a
    Struct s _     -> Scalar $ fromIntegral $ Map.size s
    Other          -> Other
    s@Sig {}       -> s
    l@Linear {}    -> l
    q@Quadratic {} -> q
    Scalar n       -> Scalar $ abs n
  negate s = fromInteger (-1) * s

instance PrimeField k => Fractional (Term k) where
  fromRational = Scalar . fromRational
  recip t = case t of
    a@Array {}   -> error $ "Cannot invert array term " ++ show a
    a@Struct {}  -> error $ "Cannot invert struct term " ++ show a
    Scalar c1    -> Scalar (recip c1)
    Other        -> Other
    Linear _     -> Other
    Quadratic {} -> Other
    Sig _        -> Other

lcScale :: PrimeField k => LC k -> k -> LC k
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: PrimeField k => LC k
lcZero = (Map.empty, 0)

linearizeSig :: PrimeField k => Signal -> Term k
linearizeSig s = Linear (Map.fromList [(s, 1)], 0)

mapSignalsInTerm :: (Signal -> Signal) -> Term k -> Term k
mapSignalsInTerm f t = case t of
    Sig s -> Sig $ f s
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Array ts -> Array $ map (mapSignalsInTerm f) ts
    Struct ts cs -> Struct (Map.map (mapSignalsInTerm f) ts) (CS.mapSignals f cs)
    Other -> Other

termSignals :: Term k -> Set Signal
termSignals t = case t of
    Sig s -> Set.insert s Set.empty
    Linear (m, _) -> Set.fromList (Map.keys m)
    Quadratic (a, _) (b, _) (c, _) -> foldr Set.union Set.empty (map (Set.fromList . Map.keys) [a, b, c])
    Scalar s -> Set.empty
    Array ts -> foldr Set.union Set.empty $ map termSignals ts
    Struct m cs -> foldr Set.union Set.empty $ map termSignals $ Map.elems m
    Other -> Set.empty


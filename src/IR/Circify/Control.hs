{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
module IR.Circify.Control where

import           Data.Natural
import           IR.SMT.TySmt
import           Codegen.C
import           GHC.Generics

-- Control flow syntax
data Control term =
    For { iter :: term, start :: term, end :: term, body :: Control term }
    | While { cond :: term, body :: Control term }
    | Call { func :: String, args :: [term] }
    | If { cond :: term, thenBody :: Control term, elseBody :: Control term }
    | Seq { left :: Control term, right :: Control term }
    | Term { inner :: term }
    | Empty {}

deriving instance Functor Control
deriving instance Applicative Control
deriving instance Monad Control
deriving instance Foldable Control
deriving instance Traversable Control
deriving instance Eq term => Eq (Control term)
deriving instance Show term => Show (Control term)
deriving instance Semigroup (Control term)
deriving instance Monoid (Control term)
deriving instance Generic term => Generic (Control term)

(\\) :: Control term -> Control term -> Control term
Empty \\ right = right
left  \\ Empty = left
left  \\ right = Seq left right
infixl 8 \\

(~<) :: TermInt -> Integer -> TermBool
a ~< b = IntBinPred IntLt a (IntLit b)
infixl 8 ~<

(~=) :: TermInt -> Integer -> TermBool
a ~= b = Eq a (IntLit b)
infixl 8 ~=

(=:) :: TermInt -> Integer -> TermBool
v =: value = Eq v (IntLit value)
infixl 8 =:

-- Take some program and extract the first loop
-- pullbackLoop (program :: Control term) ->
--    (prologue :: Control term,
--     loop     :: Control term,
--     epilogue :: Control term)
pullbackLoop :: Control t -> (Control t, Control t, Control t)
pullbackLoop l@While { .. } = (Empty, l, Empty)
pullbackLoop l@For   { .. } = (Empty, l, Empty)
pullbackLoop Seq     { .. } = case (pullbackLoop left, pullbackLoop right) of
    ((p, l, e), (p', Empty, e'))     -> (p, l, e \\ p' \\ e')              -- Loop on the left subsequence
    ((p, Empty, e), (p', l', e'))    -> (p \\ e \\ p', l', e')             -- Loop on the right subsequence
    ((p, Empty, e), (p', Empty, e')) -> (p \\ e \\ p' \\ e', Empty, Empty) -- No loop
pullbackLoop o              = (o, Empty, Empty)

-- Main loop flattening transformation
--
-- For i in (1,N) {
--    <body 1>
--    For j in (1,M) {
--      <body 2>
--    }
--    <body 3>
-- }
-- ------------------------
-- state = 1
-- i = j = 0
-- For dummy in (1, N*M) {
--   if (state == 1) {
--     if (i < N) {
--       <body 1>
--       i++;
--       j = 0
--     } else {
--       state = 2
--     }
--   }
--   if (state == 2) {
--     if (j < M) {
--       <body 2>
--       j++;
--     } else {
--       state = 3
--     }
--   }
--   if (state == 3) {
--     <body 3>
--     state = 1
--   }
-- }
loopFlatten :: Control (Term s) -> Control (Term s)
loopFlatten top = case pullbackLoop top of
    (op, For { end = n, .. }, oe) ->
        case pullbackLoop body of
            (body1, For { body = body2, end = m }, body3) ->
                "state" =: 1 \\
                i =: 0 \\
                j =: 0 \\
                For dummy (IntLit 1) (IntLit 10000) (
                    If (state ~= 1) (
                        If (i ~< n) (
                            body1 \\
                            j =: 0
                            -- i++
                        ) (state =: 2)
                    ) Empty \\
                    If (state ~= 2) (
                        If (j ~< m) (
                            body2
                            -- j++
                        ) (state =: 3)
                    ) Empty \\
                    If (state ~= 3) (
                        body3 \\
                        state =: 1
                    ) Empty
                )
            (p, Empty, e) -> p \\ e
    where dummy = Var "dummy" IntSort
          state = Var "state" IntSort
          i = Var "i" IntSort
          j = Var "j" IntSort

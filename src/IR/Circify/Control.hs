{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module IR.Circify.Control where

import           GHC.Generics
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Data.Ident

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

-- In order to do control flow flattening, this is the minimum contract the terms must follow
-- + Compare to integer
-- + Compare equality to integer
-- + Assignment, with re-assignment
-- + Increment by 1
-- + Introduce an integer literal
-- + Introduce an integer variable by name
class ControlTerm a where
    (<:)  :: a -> Integer -> a
    (==:) :: a -> Integer -> a
    (=:)  :: a -> Integer -> a
    (++:) :: a -> a
    lit   :: Integer -> a
    var   :: String -> a

nosym :: NodeInfo
nosym = OnlyPos nopos (nopos, 0)

instance ControlTerm CExpr where
    a <: b  = CBinary CLeqOp a (lit b) nosym
    a ==: b = CBinary CEqOp a (lit b) nosym
    l =: r  = CAssign CAssignOp l (lit r) nosym
    (++:) i = CUnary CPostIncOp i nosym
    lit v   = CConst $ CIntConst (CInteger v DecRepr noFlags) nosym
    var v   = CVar (Ident v 0 nosym) nosym

-- Syntactic sugar
class Sequent a b c | b -> c where
    (\\) :: a -> b -> Control c

instance Sequent CExpr CExpr CExpr where
    a \\ b = Seq (Term a) (Term b)

instance Sequent CExpr (Control CExpr) CExpr where
    a \\ b = Seq (Term a) b

instance Sequent (Control t) (Control t) t where
    Empty \\ right = right
    left  \\ Empty = left
    left  \\ right = Seq left right

infixl 6 \\
infixl 8 <:
infixl 8 ==:
infixl 7 =:

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
loopFlatten :: ControlTerm t => Control t -> Control t
loopFlatten top = case pullbackLoop top of
    (op, For { end = n, .. }, oe) ->
        case pullbackLoop body of
            (body1, For { body = body2, end = m }, body3) ->
                state =: 1 \\
                i =: 0 \\
                j =: 0 \\
                For dummy (lit 1) (lit 10000) (
                    If (state ==: 1) (
                        If (i <: n) (
                            body1 \\
                            (j =: 0) \\
                            (i ++:)
                        ) (state =: 2)
                    ) Empty \\
                    If (state ==: 2) (
                        If (j <: m) (
                            body2 \\
                            (j ++:)
                        ) (state =: 3)
                    ) Empty \\
                    If (state ==: 3) (
                        body3 \\
                        state =: 1
                    ) Empty
                )
            (p, Empty, e) -> p \\ e
    where dummy = var "dummy"
          state = var "state"
          i = var "i"
          j = var "j"

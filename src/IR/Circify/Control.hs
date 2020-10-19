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
import           Util.Cfg

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
deriving instance Foldable Control
deriving instance Traversable Control
deriving instance Eq term => Eq (Control term)
deriving instance Show term => Show (Control term)
instance Semigroup (Control term) where
    Empty <> right = right
    left <> Empty = left
    left <> right = Seq left right

instance Monoid (Control term) where
    mempty = Empty
deriving instance Generic term => Generic (Control term)

-- In order to do control flow flattening, this is the minimum contract the terms must follow
-- + Compare to integer
-- + Compare equality to integer
-- + Assignment, with re-assignment
-- + Increment by 1
-- + Introduce an integer literal
-- + Introduce an integer variable by name
class ControlTerm a where
    (<:)  :: a -> a -> a
    (==:) :: a -> a -> a
    (=:)  :: a -> Integer -> Control a
    (++:) :: a -> Control a
    lit   :: Integer -> a
    var   :: String -> a

nosym :: NodeInfo
nosym = OnlyPos nopos (nopos, 0)

instance ControlTerm CExpr where
    a <: b  = CBinary CLeqOp a b nosym
    a ==: b = CBinary CEqOp a b nosym
    l =: r  = Term $ CAssign CAssignOp l (lit r) nosym
    (++:) i = Term $ CUnary CPostIncOp i nosym
    lit v   = CConst $ CIntConst (CInteger v DecRepr noFlags) nosym
    var v   = CVar (Ident v 0 nosym) nosym

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
    ((p, l, e), (p', Empty, e'))     -> (p, l, e <> p' <> e')              -- Loop on the left subsequence
    ((p, Empty, e), (p', l', e'))    -> (p <> e <> p', l', e')             -- Loop on the right subsequence
    ((p, Empty, e), (p', Empty, e')) -> (p <> e <> p' <> e', Empty, Empty) -- No loop
pullbackLoop o              = (o, Empty, Empty)

-- Main loop flattening transformation
--   Wahby et al. "Efficient RAM and control flow in verifiable outsourced computation"
loopFlatten' :: ControlTerm t => Integer -> Control t -> Control t
loopFlatten' maxIteration While { .. } =
    case pullbackLoop body of
       (body1, For { body = body2, end = m }, body3) ->
            state =: 1 <>
            For dummy (lit 1) (lit maxIteration) (
                If (state ==: lit 1) (
                    If (cond) (
                        body1 <>
                        i =: 0 <>
                        state =: 2
                    ) (state =: 3)
                ) Empty <>
                If (state ==: lit 2) (
                    If (i <: m) (
                        body2 <>
                        (i ++:)
                    ) (state =: 2)
                ) Empty <>
                If (state ==: lit 3) (
                    body3 <>
                    state =: 1
                ) Empty
            )
       (p, Empty, e) -> p <> e
       where dummy = var "dummy"
             state = var "state"
             i = var "i"
loopFlatten' maxIteration For { end = n, .. } =
    case pullbackLoop body of
        (body1, For { body = body2, end = m }, body3) ->
            state =: 1 <>
            i =: 0 <>
            j =: 0 <>
            For dummy (lit 1) (lit maxIteration) (
                If (state ==: lit 1) (
                    If (i <: n) (
                        body1 <>
                        j =: 0 <>
                        (i ++:)
                    ) (state =: 2)
                ) Empty <>
                If (state ==: lit 2) (
                    If (j <: m) (
                        body2 <>
                        (j ++:)
                    ) (state =: 3)
                ) Empty <>
                If (state ==: lit 3) (
                    body3 <>
                    state =: 1
                ) Empty
            )
        (p, Empty, e) -> p <> e
    where dummy = var "dummy"
          state = var "state"
          i = var "i"
          j = var "j"

loopFlatten :: ControlTerm t => Control t -> Cfg (Control t)
loopFlatten top = do
    maxIteration <- asks (Util.Cfg._loopMaxIteration)
    return $ case pullbackLoop top of
        (op, loop, ep) -> op <> loopFlatten' maxIteration loop <> ep


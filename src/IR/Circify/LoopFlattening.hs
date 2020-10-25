{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
module IR.Circify.LoopFlattening where

import           Control.Monad.Reader           ( asks )
import           Control.Monad.State.Strict
import           Util.Cfg
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as M
import           Data.Natural
import           IR.Circify.Control

-- Take some program and extract the first loop
-- pullbackLoop (program :: Control term) ->
--    (prologue :: Control term,
--     loop     :: Control term,
--     epilogue :: Control term)
pullbackLoop :: Control t -> (Control t, Control t, Control t)
pullbackLoop l@While {..} = (Empty, l, Empty)
pullbackLoop l@For {..}   = (Empty, l, Empty)
pullbackLoop Seq {..}     = case (pullbackLoop left, pullbackLoop right) of
  ((p, l    , e), (p', Empty, e')) -> (p, l, e <> p' <> e')              -- Loop on the left subsequence
  ((p, Empty, e), (p', l', e')   ) -> (p <> e <> p', l', e')             -- Loop on the right subsequence
  ((p, Empty, e), (p', Empty, e')) -> (p <> e <> p' <> e', Empty, Empty) -- No loop
pullbackLoop o = (o, Empty, Empty)

-- Need a context for variable shadowing and a Cfg for arguments
type LoopAnalysis m = StateT (Map String Natural) Cfg m

-- Utility function that allows for reference tracking
newIdentifier :: Monad m => String -> StateT (Map String Natural) m String
newIdentifier identifier = do
  dict <- get
  case M.insertLookupWithKey (const (+)) identifier 1 dict of
    (Nothing, newdict) -> do
      put newdict
      return identifier
    (Just v, newdict) -> do
      put newdict
      return $ identifier ++ (show v)

-- Main loop flattening transformation
--   Wahby et al. "Efficient RAM and control flow in verifiable outsourced computation"
mkLoop
  :: (ControlTerm t, Monad m)
  => t
  -> t
  -> t
  -> Control t
  -> Control t
  -> Control t
  -> Control t
  -> StateT (Map String Natural) m (Control t)
mkLoop maxIteration outterCond innerCond prologue body1 body2 body3 = do
    -- Need different versions of these to avoid shadowing
  dummy <- var <$> newIdentifier "dummy"
  state <- var <$> newIdentifier "state"
  return $ state =: 0 <> prologue <> For
    dummy
    (lit 0)
    maxIteration
    (  If (state ==: lit 0)
          (If (outterCond) (body1 <> state =: 1) (state =: 3))
          Empty
    <> If (state ==: lit 1) (If (innerCond) (body2) (state =: 2)) Empty
    <> If (state ==: lit 2) (body3 <> state =: 0)                 Empty
    )

loopFlatten :: ControlTerm t => Control t -> LoopAnalysis (Control t)
loopFlatten top = do
  maxIteration <- asks (Util.Cfg._loopMaxIteration)
  case (pullbackLoop top) of
    (_ , Empty     , _ ) -> return top
    (op, While {..}, ep) ->                                           -- Top level (While)
                            case (pullbackLoop body) of
      (_    , Empty, _    ) -> return top
      (body1, For { body = body2, end = m, iter = i, ..}, body3) ->  -- Inner loop (For)
                                                                    do
        flatbody2 <- loopFlatten body2
        flatbody3 <- loopFlatten body3
        flatep    <- loopFlatten ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration)              -- Maximum iteration used for while loops as upper-bound
                            cond                                          -- Outer loop conditional
                            (i <: m)                                      -- Inner loop conditional (for loop, (i < m))
                            (i =: 0)                                      -- Prologue statements (allocate i in the stack)
                            body1                                         -- Outer loop body, before inner loop starts
                            (flatbody2 <> (i ++:))                        -- Inner loop body (for lopp, do i++)
                            flatbody3                                     -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
      (body1, While { cond = condInner, body = body2, ..}, body3) -> -- Inner loop (While)
                                                                     do
        flatbody2 <- loopFlatten body2
        flatbody3 <- loopFlatten body3
        flatep    <- loopFlatten ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration)              -- Maximum iteration used for while loops as upper-bound
                            cond                                          -- Outer loop conditional
                            condInner                                     -- Inner loop conditional
                            Empty                                         -- Prologue statements (allocate i in the stack)
                            body1                                         -- Outer loop body, before inner loop starts
                            flatbody2                                     -- Inner loop body
                            flatbody3                                     -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep
    (op, For { end = n, iter = j, ..}, ep) ->                          -- Top level (For)
                                              case (pullbackLoop body) of
      (_    , Empty, _    ) -> return top
      (body1, For { body = body2, end = m, iter = i, ..}, body3) ->  -- Inner loop (For)
                                                                    do
        flatbody2 <- loopFlatten body2
        flatbody3 <- loopFlatten body3
        flatep    <- loopFlatten ep
        flatloop  <- mkLoop (n *: m)                                      -- Maximum iteration (n*m)
                            (j <: n)                                      -- Outer loop conditional (for loop, (j < n))
                            (i <: m)                                      -- Inner loop conditional (for loop, (i < m))
                            (j =: 0 <> i =: 0)                            -- Prologue statements (allocate i, j in the stack)
                            (body1 <> (i =: 0))                           -- Outer loop body, before inner loop starts
                            (flatbody2 <> (i ++:))                        -- Inner loop body (for lopp, do i++)
                            (flatbody3 <> (j ++:))                        -- Outer loop body, after inner loop returns (for loop, do j++)
        return $ op <> flatloop <> flatep
      (body1, While { body = body2, ..}, body3) ->                   -- Inner loop (While)
                                                   do
        flatbody2 <- loopFlatten body2
        flatbody3 <- loopFlatten body3
        flatep    <- loopFlatten ep
        flatloop  <- mkLoop (lit . toInteger $ maxIteration)              -- Maximum iteration used for while loops as upper-bound
                            (j <: n)                                      -- Outer loop conditional (for loop, j < n)
                            cond                                          -- Inner loop conditional
                            (j =: 0)                                      -- Prologue statements (allocate j in the stack)
                            body1                                         -- Outer loop body, before inner loop starts
                            flatbody2                                         -- Inner loop body
                            (flatbody3 <> (j ++:))                             -- Outer loop body, after inner loop returns
        return $ op <> flatloop <> flatep

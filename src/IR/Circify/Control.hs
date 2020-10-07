{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module IR.Circify.Control where

import Data.Natural
import Data.String.Interpolate (i)
import Control.Monad.State.Lazy

newtype Range = Range { start :: Int, end :: Int, step :: Int, iter :: term Int }

mkRange :: Int -> Int -> Int -> Range
mkRange start end step iter
    | abs (end - start) >= step && step > 0 = Range start end step iter
    | abs (end - start) >= (- step) && step < 0 = Range start end step iter
    | otherwise = error [i| Bad loop bounds for(i = #{start}; i < #{end}; i = i + #{step})|]

-- Control flow semantics
data Control (a :: *) (term :: * -> *) where
    For :: Range -> Control a term -> Control a term
    While :: term Bool -> Natural -> Control a term -> Control a term
    If :: term Bool -> Control a term -> Control a term -> Control a term
    One :: term a -> Control a term
    Seq :: term b -> Control a term -> Control a term
    deriving (Functor, Applicative, Monad, Traversable)

-- Convenience constructors
forloop :: Range -> Control a term -> Control a term
forloop range body = For range body

whileloop :: term Bool -> Natural -> Control a term -> Control a term
whileloop check maxIterations body = While check maxIterations body

ifstmt :: term Bool -> Control a term -> Control a term -> Control a term
ifstmt check left right = If check left right

data LoopAnalysis a term = LoopAnalysis { stack :: Control a term }

loopAnalysis :: Control a term -> State (LoopAnalysis a term) (Control a term)
loopAnalysis = undefined

loopFlatten :: Control a term -> Control a term
loopFlatten top = undefined



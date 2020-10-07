{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module IR.Circify.Control where

import           Data.Natural
import           Data.String.Interpolate   (i)
import           Control.Monad.State.Lazy

-- Range of a for loop
data Range = Range { start :: Int, end :: Int, step :: Int }

-- Convenience function that errors on bad ranges
mkRange :: Int -> Int -> Int -> Range
mkRange start end step
    | abs (end - start) >= step && step > 0 = Range start end step
    | abs (end - start) >= (- step) && step < 0 = Range start end step
    | otherwise = error [i| Bad loop bounds for(i = #{start}; i < #{end}; i = i + #{step})|]

-- Control flow syntax
data Control (term :: * -> *) (a :: *) where
    For :: Range -> term Int -> Control term a -> Control term a
    While :: term Bool -> Natural -> Control term a -> Control term a
    If :: term Bool -> Control term a -> Control term a -> Control term a
    One :: term a -> Control term a
    Seq :: term b -> Control term a -> Control term a

deriving instance Functor term => Functor (Control term)
deriving instance Applicative term => Applicative (Control term)
deriving instance Monad term => Monad (Control term)

-- Convenience constructors
forloop :: Range -> term Int -> Control term a -> Control term a
forloop range iterator body = For range iterator body

whileloop :: term Bool -> Natural -> Control term a -> Control term a
whileloop check maxIterations body = While check maxIterations body

ifstmt :: term Bool -> Control term a -> Control term a -> Control term a
ifstmt check left right = If check left right

-- Analysis state
data LoopAnalysis a term = LoopAnalysis { stack :: [Control term a] }

-- TODO: Implement analysis and loop flattening transformation
loopAnalysis :: Control term a -> State (LoopAnalysis a term) (Control term a)
loopAnalysis = undefined

loopFlatten :: Control term a -> Control term a
loopFlatten top = undefined



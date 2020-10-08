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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module IR.Circify.Control where

import           Data.Natural
import           Data.String.Interpolate   (i)
import           Control.Monad.State.Lazy

-- Control flow syntax
data Control term a =
    For { maxit :: Int, init :: term, step :: term, cond :: term, body :: Control term a }
    | While { maxit :: Int, cond :: term, body :: Control term a }
    | Call { func :: String, args :: [term] }
    | Empty {}
    | Seq { left :: term, right :: Control term a }

deriving instance Functor (Control term)
deriving instance Applicative (Control term)
deriving instance Monad (Control term)
deriving instance Foldable (Control term)
deriving instance Traversable (Control term)

-- Analysis state
data LoopAnalysis a term = LoopAnalysis { stack :: [Control term a] }

-- TODO: Implement analysis and loop flattening transformation
loopAnalysis :: Control term a -> State (LoopAnalysis a term) (Control term a)
loopAnalysis = undefined

loopFlatten :: Control term a -> Control term a
loopFlatten top = undefined



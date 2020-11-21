module Targets.BackEnd
  ( BackEnd
  )
where

import qualified IR.SMT.Assert                 as Assert
import           Util.Log

type BackEnd o = Assert.AssertState -> Log o

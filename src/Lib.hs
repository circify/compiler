module Lib
    ( someFunc
    ) where
import           Parser.LIR
import           Parser.MIR

someFunc :: IO ()
someFunc = printLICM --printRegAlloc

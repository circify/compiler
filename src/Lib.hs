module Lib
    ( someFunc
    ) where
import           Parser.LIR
import           Parser.MIR
import           Parser.XML

someFunc :: IO ()
someFunc = parseXMLDir "ISA_v82A_A64_xml_00bet3.1/"-- printLICM --printRegAlloc

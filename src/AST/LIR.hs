{-# LANGUAGE DeriveGeneric #-}
module AST.LIR ( module AST.LIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Data.Aeson
import           Data.Text
import           Data.Word
import           GHC.Generics

{-|

This module is an AST for IonMonkey's LIR.
It works with the LIR opcodes presented here (and other arch-specific files):
https://searchfox.org/mozilla-central/source/__GENERATED__/__macosx64__/js/src/jit/LOpcodes.h#10

There are no semantics for this IR, so we go on the implementation at:
https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#6427

-}

type LBlockId = Word32
-- type LNodeId = Word32
-- type VirtualRegister = Word32
-- type RegisterCode = Word32
-- type ConstantIndex = Word32
-- type StackSlot = Word32
-- type ArgumentIndex = Word32

-- data RegisterName = GeneralRegisterName GeneralRegisterName
--                   | FloatRegisterName FloatRegisterName
--                   deriving (Generic, Show)

-- data GeneralRegisterName = GName String {- ... -}
--                          deriving (Generic, Show)
-- data FloatRegisterName = FName String {- ... -}
--                        deriving (Generic, Show)

data LIR = LIR { blocks :: [LBlock] }
         deriving (Generic, Show)

instance ToJSON LIR where
instance FromJSON LIR where

data LBlock = LBlock { blockId :: LBlockId
                     , nodes   :: [LNode]
                     }
            deriving (Generic, Show)

instance ToJSON LBlock where
instance FromJSON LBlock where

data LNode = LNode deriving (Generic, Show)

instance ToJSON LNode where
instance FromJSON LNode where

-- data LNode = LNode { nodeId            :: LNodeId
--                    , opName            :: LOperand
--                    , extraName         :: Maybe Text
--                    , isCall            :: Bool
--                    , recoversInput     :: Bool
--                    , callPreservesRegs :: [RegisterName] -- empty for all but some WASM stuff
--                    , operands          :: [LAllocation]
--                    , defs              :: [LDefinition]
--                    , temps             :: [LDefinition]
--                    , successors        :: [LBlockId]
--                    }
--            deriving (Generic, Show)

-- data LOperand = LOperand Text
--               deriving (Generic, Show)

-- data LDefinition = LDefinition { defVirtualRegister :: VirtualRegister
--                                , ty                 :: LDefinitionType
--                                , policy             :: LDefinitionPolicy
--                                , output             :: Maybe LAllocation
--                                }
--                  deriving (Generic, Show)

-- data LDefinitionType = General
--                      | Int32
--                      | Object
--                      | Slots
--                      | Float32
--                      | Double
--                      | SIMD32Int
--                      | SIMD128Float
--                      | Type
--                      | Payload
--                      | Box
--                        deriving (Generic, Show)

-- data LDefinitionPolicy = DefFixed
--                        | DefRegister
--                        | MustReuseInput
--                          deriving (Generic, Show)

-- data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
--                  | {- kind = "constantIndex" -} LConstantIndexAllocation LConstantIndex
--                  | {- kind = "use" -} LUseAllocation LUse
--                  | {- kind = "gpr" -} LGeneralRegAllocation LGeneralReg
--                  | {- kind = "fpu" -} LFloatRegAllocation LFloatReg
--                  | {- kind = "stackSlot" -} LStackSlotAllocation LStackSlot
--                  | {- kind = "argumentSlot" -} LArgumentAllocation LArgument
--                    deriving (Generic, Show)

-- data LConstantValue = LConstantValue -- TODO; empty for now
--                     deriving (Generic, Show)

-- data LConstantIndex = LConstantIndex { constIdx :: ConstantIndex }
--                     deriving (Generic, Show)

-- data LUse = LUse { usePolicy          :: LUsePolicy
--                  , useVirtualRegister :: VirtualRegister
--                  , registerCode       :: RegisterCode
--                  , usedAtStart        :: Bool
--                  }
--           deriving (Generic, Show)

-- data LUsePolicy = Any
--                 | UseRegister
--                 | UseFixed
--                 | KeepAlive
--                 | RecoveredInput
--                   deriving (Generic, Show)

-- data LGeneralReg = LGeneralReg { greg :: GeneralRegisterName }
--                  deriving (Generic, Show)

-- data LFloatReg = LFloatReg { freg :: FloatRegisterName }
--                deriving (Generic, Show)

-- data LStackSlot = LStackSlot { slot :: StackSlot }
--                 deriving (Generic, Show)

-- data LArgument = LArgument { argIdx :: ArgumentIndex }
--                deriving (Generic, Show)


























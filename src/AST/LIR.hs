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
type LNodeId = Word32
type VirtualRegister = Word32
type RegisterCode = Word32
type ConstantIndex = Word32
type StackSlot = Word32
type ArgumentIndex = Word32

data RegisterName = GeneralRegisterName GeneralRegisterName
                  | FloatRegisterName FloatRegisterName
                  deriving (Generic, Show)

instance ToJSON RegisterName where
instance FromJSON RegisterName where

data GeneralRegisterName = GName String {- ... -}
                         deriving (Generic, Show)
data FloatRegisterName = FName String {- ... -}
                       deriving (Generic, Show)

instance ToJSON FloatRegisterName where
instance FromJSON FloatRegisterName where
instance ToJSON GeneralRegisterName where
instance FromJSON GeneralRegisterName where

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

data LNode = LNode { nodeId            :: LNodeId
                   , opName            :: LOperand
                   , extraName         :: Maybe Text
                   , isCall            :: Bool
                   , recoversInput     :: Bool
                   , callPreservesRegs :: [RegisterName] -- empty for all but some WASM stuff
                   , operands          :: [LAllocation]
                   , defs              :: [LDefinition]
                   , temps             :: [LDefinition]
                   , successors        :: [LBlockId]
                   }
           deriving (Generic, Show)

instance ToJSON LNode where
instance FromJSON LNode where

data LOperand = LOperand Text
              deriving (Generic, Show)

instance ToJSON LOperand where
instance FromJSON LOperand where

data LDefinition = LDefinition { defVirtualRegister :: VirtualRegister
                               , ty                 :: LDefinitionType
                               , policy             :: LDefinitionPolicy
                               , output             :: Maybe LAllocation
                               }
                 deriving (Generic, Show)

instance ToJSON LDefinition where
instance FromJSON LDefinition where

data LDefinitionType = General
                     | Int32
                     | Object
                     | Slots
                     | Float32
                     | Double
                     | SIMD32Int
                     | SIMD128Float
                     | Type
                     | Payload
                     | Box
                       deriving (Generic, Show)

instance ToJSON LDefinitionType where
instance FromJSON LDefinitionType where

data LDefinitionPolicy = DefFixed
                       | DefRegister
                       | MustReuseInput
                         deriving (Generic, Show)

instance ToJSON LDefinitionPolicy where
instance FromJSON LDefinitionPolicy where

data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
                 | {- kind = "constantIndex" -} LConstantIndexAllocation LConstantIndex
                 | {- kind = "use" -} LUseAllocation LUse
                 | {- kind = "gpr" -} LGeneralRegAllocation LGeneralReg
                 | {- kind = "fpu" -} LFloatRegAllocation LFloatReg
                 | {- kind = "stackSlot" -} LStackSlotAllocation LStackSlot
                 | {- kind = "argumentSlot" -} LArgumentAllocation LArgument
                   deriving (Generic, Show)

instance ToJSON LAllocation where
instance FromJSON LAllocation where

data LConstantValue = LConstantValue -- TODO; empty for now
                    deriving (Generic, Show)

instance ToJSON LConstantValue where
instance FromJSON LConstantValue where

data LConstantIndex = LConstantIndex { constIdx :: ConstantIndex }
                    deriving (Generic, Show)

instance ToJSON LConstantIndex where
instance FromJSON LConstantIndex where

data LUse = LUse { usePolicy          :: LUsePolicy
                 , useVirtualRegister :: VirtualRegister
                 , registerCode       :: RegisterCode
                 , usedAtStart        :: Bool
                 }
          deriving (Generic, Show)

instance ToJSON LUse where
instance FromJSON LUse where

data LUsePolicy = Any
                | UseRegister
                | UseFixed
                | KeepAlive
                | RecoveredInput
                  deriving (Generic, Show)

instance ToJSON LUsePolicy where
instance FromJSON LUsePolicy where

data LGeneralReg = LGeneralReg { greg :: GeneralRegisterName }
                 deriving (Generic, Show)

instance ToJSON LGeneralReg where
instance FromJSON LGeneralReg where

data LFloatReg = LFloatReg { freg :: FloatRegisterName }
               deriving (Generic, Show)

instance ToJSON LFloatReg where
instance FromJSON LFloatReg where

data LStackSlot = LStackSlot { slot :: StackSlot }
                deriving (Generic, Show)

instance ToJSON LStackSlot where
instance FromJSON LStackSlot where

data LArgument = LArgument { argIdx :: ArgumentIndex }
               deriving (Generic, Show)

instance ToJSON LArgument where
instance FromJSON LArgument where
























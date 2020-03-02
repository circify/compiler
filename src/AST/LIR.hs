module AST.LIR ( module AST.LIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Data.Text
import           Data.Word

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

data GeneralRegisterName = GName String {- ... -}
data FloatRegisterName = FName String {- ... -}

data LIR = LIR { blocks :: [LBlock] }

data LBlock = LBlock { blockId :: LBlockId
                     , nodes   :: [LNode]
                     }

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

data LOperand = LOperand Text

data LDefinition = LDefinition { defVirtualRegister :: VirtualRegister
                               , ty                 :: LDefinitionType
                               , policy             :: LDefinitionPolicy
                               , output             :: Maybe LAllocation
                               }

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

data LDefinitionPolicy = DefFixed
                       | DefRegister
                       | MustReuseInput

data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
                 | {- kind = "constantIndex" -} LConstantIndexAllocation LConstantIndex
                 | {- kind = "use" -} LUseAllocation LUse
                 | {- kind = "gpr" -} LGeneralRegAllocation LGeneralReg
                 | {- kind = "fpu" -} LFloatRegAllocation LFloatReg
                 | {- kind = "stackSlot" -} LStackSlotAllocation LStackSlot
                 | {- kind = "argumentSlot" -} LArgumentAllocation LArgument

data LConstantValue = LConstantValue -- TODO; empty for now

data LConstantIndex = LConstantIndex { constIdx :: ConstantIndex }

data LUse = LUse { usePolicy          :: LUsePolicy
                 , useVirtualRegister :: VirtualRegister
                 , registerCode       :: RegisterCode
                 , usedAtStart        :: Bool
                 }

data LUsePolicy = Any
                | UseRegister
                | UseFixed
                | KeepAlive
                | RecoveredInput

data LGeneralReg = LGeneralReg { greg :: GeneralRegisterName }

data LFloatReg = LFloatReg { freg :: FloatRegisterName }

data LStackSlot = LStackSlot { slot :: StackSlot }

data LArgument = LArgument { argIdx :: ArgumentIndex }


























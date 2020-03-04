{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module AST.LIR ( module AST.LIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Control.Monad.State.Strict (unless, when)
import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  hiding (map)
import           Data.Word
import           GHC.Generics

{-|

This module is an AST for IonMonkey's LIR.
It works with the LIR opcodes presented here (and other arch-specific files):
https://searchfox.org/mozilla-central/source/__GENERATED__/__macosx64__/js/src/jit/LOpcodes.h#10

There are no semantics for this IR, so we go on the implementation at:
https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#6427

-}

data LOperand = LOperand
              deriving (Show, Generic)

instance ToJSON LOperand where
instance FromJSON LOperand where

type LBlockId = Word32
type LNodeId = Word32
type VirtualRegister = Word32
type RegisterCode = Word32
type ConstantIndex = Word32
type StackSlot = Word32
type ArgumentIndex = Word32

data RegisterName = GeneralName GeneralRegisterName
                  | FloatName FloatRegisterName
                    deriving (Show, Generic)

data GeneralRegisterName =  GeneralRegisterName {- ... -}
                         deriving (Show, Generic)
data FloatRegisterName = FloatRegisterName {- ... -}
                      deriving (Show, Generic)

-- data LIR = LIR { blocks :: [LBlock] }
--          deriving (Show, Generic)

-- data LBlock = LBlock { id    :: LBlockId
--                      , nodes :: [LNode]
--                      }

-- data LNode = LNode { id                :: LNodeId
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

-- data LDefinition = LDefinition { virtualRegister :: VirtualRegister
--                                , ty              :: LDefinitionType
--                                , policy          :: LDefinitionPolicy
--                                , output          :: Maybe LAllocation
--                                }

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

-- data LDefinitionPolicy = DefFixed
--                        | DefRegister
--                        | MustReuseInput

-- instance Show LDefinitionPolicy where
--   show DefFixed       = "fixed"
--   show DefRegister    = "register"
--   show MustReuseInput = "mustReuseInput"x

data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
                 -- | {- kind = "constantIndex" -} LConstantIndexAllocation LConstantIndex
                 -- | {- kind = "use" -} LUseAllocation LUse
                 -- | {- kind = "gpr" -} LGeneralRegAllocation LGeneralReg
                 -- | {- kind = "fpu" -} LFloatRegAllocation LFloatReg
                 -- | {- kind = "stackSlot" -} LStackSlotAllocation LStackSlot
                 -- | {- kind = "argumentSlot" -} LArgumentAllocation LArgument
                   deriving (Generic)

instance Show LAllocation where
    show LConstantValueAllocation = "constantValue"

instance ToJSON LAllocation where
instance FromJSON LAllocation where
    parseJSON (Object obj) = do
      let kindField = HM.lookup "kind" obj
      unless (isJust kindField) $ error "No kind field in LAllocation object"
      case fromJust kindField of
        "constantValue" -> return LConstantValueAllocation
        _ -> error "Unexpected kind field in LAllocation object "

data Kinder = Kinder { kind :: String }
            deriving (Show, Generic)

instance ToJSON Kinder where
instance FromJSON Kinder where

-- data LConstantValue = LConstantValue -- TODO; empty for now

-- data LConstantIndex = LConstantIndex { index :: ConstantIndex }

data LUse = LUse { policy          :: LUsePolicy
                 , virtualRegister :: VirtualRegister
                 , registerCode    :: RegisterCode
                 , usedAtStart     :: Bool
                 }
          deriving (Show, Generic)

instance ToJSON LUse where
instance FromJSON LUse where

data LUsePolicy = Any
                | UseRegister
                | UseFixed
                | KeepAlive
                | RecoveredInput
                  deriving (Show, Generic)

instance ToJSON LUsePolicy where
instance FromJSON LUsePolicy where

-- instance Show LUsePolicy where
--   show Any            = "any"
--   show UseRegister    = "register"
--   show UseFixed       = "fixed"
--   show KeepAlive      = "keepAlive"
--   show RecoveredInput = "recoveredInput"

-- data LGeneralReg = LGeneralReg { reg :: GeneralRegisterName }

-- data LFloatReg = LFloatReg { reg :: FloatRegisterName }

-- data LStackSlot = LStackSlot { slot :: StackSlot }

data LArgument = LArgument { index :: ArgumentIndex }
               deriving (Show, Generic)

instance ToJSON LArgument where
instance FromJSON LArgument where

---
---
---
---

-- type LBlockId = Word32
-- type LNodeId = Word32
-- type VirtualRegister = Word32
-- type RegisterCode = Word32
-- type ConstantIndex = Word32
-- type StackSlot = Word32
-- type ArgumentIndex = Word32

-- data RegisterName = GeneralRegisterName GeneralRegisterName
--                   | FloatRegisterName FloatRegisterName
--                   deriving (Generic, Show)

-- instance ToJSON RegisterName where
-- instance FromJSON RegisterName where

-- data GeneralRegisterName = GName String {- ... -}
--                          deriving (Generic, Show)
-- data FloatRegisterName = FName String {- ... -}
--                        deriving (Generic, Show)

-- instance ToJSON FloatRegisterName where
-- instance FromJSON FloatRegisterName where
-- instance ToJSON GeneralRegisterName where
-- instance FromJSON GeneralRegisterName where

-- data LIR = LIR { blocks :: [LBlock] }
--          deriving (Generic, Show)

-- instance ToJSON LIR where
-- instance FromJSON LIR where

-- data LBlock = LBlock { blockId :: LBlockId -- id
--                      , nodes   :: [LNode]
--                      }
--             deriving (Generic, Show)

-- modifyLBlockFields :: String -> String
-- modifyLBlockFields f = if f == "id" then "blockId" else f

-- instance ToJSON LBlock where
-- instance FromJSON LBlock where
--   parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modifyLBlockFields }

-- data LNode = LNode { nodeId            :: LNodeId -- id
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

-- modifyLNodeFields :: String -> String
-- modifyLNodeFields f = if f == "id" then "nodeId" else f

-- instance ToJSON LNode where
--   -- toJSON = genericToJSON defaultOptions { fieldLabelModifier = modifyLNodeFields }
-- instance FromJSON LNode where
--   parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = modifyLNodeFields }

-- data LOperand = LOperand Text
--               deriving (Generic, Show)

-- instance ToJSON LOperand where
-- instance FromJSON LOperand where

-- data LDefinition = LDefinition { defVirtualRegister :: VirtualRegister
--                                , ty                 :: LDefinitionType
--                                , policy             :: LDefinitionPolicy
--                                , output             :: Maybe LAllocation
--                                }
--                  deriving (Generic, Show)

-- modifyLDefinitionFields :: String -> String
-- modifyLDefinitionFields f = if f == "type" then "ty" else f

-- instance ToJSON LDefinition where
-- instance FromJSON LDefinition where
--   parseJSON = genericParseJSON defaultOptions
--               { fieldLabelModifier = modifyLDefinitionFields }

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

-- instance ToJSON LDefinitionType where
-- instance FromJSON LDefinitionType where

-- data LDefinitionPolicy = DefFixed
--                        | DefRegister
--                        | MustReuseInput
--                          deriving (Generic)

-- instance Show LDefinitionPolicy where
--   show DefFixed       = "fixed"
--   show DefRegister    = "register"
--   show MustReuseInput = "mustReuseInput"

-- instance ToJSON LDefinitionPolicy where
-- instance FromJSON LDefinitionPolicy where

-- data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
--                  | {- kind = "constantIndex" -} LConstantIndexAllocation LConstantIndex
--                  | {- kind = "use" -} LUseAllocation LUse
--                  | {- kind = "gpr" -} LGeneralRegAllocation LGeneralReg
--                  | {- kind = "fpu" -} LFloatRegAllocation LFloatReg
--                  | {- kind = "stackSlot" -} LStackSlotAllocation LStackSlot
--                  | {- kind = "argumentSlot" -} LArgumentAllocation LArgument
--                    deriving (Generic, Show)

-- instance ToJSON LAllocation where
-- instance FromJSON LAllocation where

-- data LConstantValue = LConstantValue -- TODO; empty for now
--                     deriving (Generic, Show)

-- instance ToJSON LConstantValue where
-- instance FromJSON LConstantValue where

-- data LConstantIndex = LConstantIndex { constIdx :: ConstantIndex }
--                     deriving (Generic, Show)

-- modifyLConstantIndexFields :: String -> String
-- modifyLConstantIndexFields _ = "id"

-- instance ToJSON LConstantIndex where
-- instance FromJSON LConstantIndex where
--   parseJSON = genericParseJSON defaultOptions
--               { fieldLabelModifier = modifyLConstantIndexFields }

-- data LUse = LUse { usePolicy          :: LUsePolicy
--                  , useVirtualRegister :: VirtualRegister
--                  , registerCode       :: RegisterCode
--                  , usedAtStart        :: Bool
--                  }
--           deriving (Generic, Show)

-- instance Show LUse where
--   show LUsePolicy      = "use"
--   show VirtualRegister = ""

-- instance ToJSON LUse where
-- instance FromJSON LUse where

-- data LUsePolicy = Any
--                 | UseRegister
--                 | UseFixed
--                 | KeepAlive
--                 | RecoveredInput
--                   deriving (Generic, Show)

-- instance ToJSON LUsePolicy where
-- instance FromJSON LUsePolicy where

-- data LGeneralReg = LGeneralReg { greg :: GeneralRegisterName }
--                  deriving (Generic, Show)

-- instance ToJSON LGeneralReg where
-- instance FromJSON LGeneralReg where

-- data LFloatReg = LFloatReg { freg :: FloatRegisterName }
--                deriving (Generic, Show)

-- instance ToJSON LFloatReg where
-- instance FromJSON LFloatReg where

-- data LStackSlot = LStackSlot { slot :: StackSlot }
--                 deriving (Generic, Show)

-- instance ToJSON LStackSlot where
-- instance FromJSON LStackSlot where

-- data LArgument = LArgument { argIdx :: ArgumentIndex }
--                deriving (Generic, Show)

-- instance ToJSON LArgument where
-- instance FromJSON LArgument where
























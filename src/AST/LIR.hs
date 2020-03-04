{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module AST.LIR ( module AST.LIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Control.Monad.State.Strict (unless, when)
import           Data.Aeson                 hiding (Object)
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

type LOperand = Text
type LBlockId = Word32
type LNodeId = Word32
type VirtualRegister = Word32
type RegisterCode = Word32
type ConstantIndex = Word32
type StackSlot = Word32
type ArgumentIndex = Word32
type RegisterName = Text

data LIR = LIR { blocks :: [LBlock] }
         deriving (Show, Generic)

instance FromJSON LIR where

data LBlock = LBlock { id    :: LBlockId
                     , nodes :: [LNode]
                     }
            deriving (Show, Generic)

instance FromJSON LBlock where

data LNode = LNode { id                :: LNodeId
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

instance FromJSON LNode where

data LDefinition = LDefinition { virtualRegister :: VirtualRegister
                               , ty              :: LDefinitionType
                               , policy          :: LDefinitionPolicy
                               , output          :: Maybe LAllocation
                               }
                 deriving (Show)

instance FromJSON LDefinition where
    parseJSON = withObject "definition" $ \o -> do
      vr <- o .: ("virtualRegister" :: Text)
      ty <- o .: ("type" :: Text)
      -- generics are broken....?
      let ty' = case ty of
                  "general"      -> General
                  "int32"        -> Int32
                  "object"       -> Object
                  "slots"        -> Slots
                  "float32"      -> Float32
                  "double"       -> Double
                  "simd32int"    -> SIMD32Int
                  "simd128float" -> SIMD128Float
                  "type"         -> Type
                  "payload"      -> Payload
                  "box"          -> Box
                  _              -> error ty
      pol <- o .: ("policy" :: Text)
      -- you're probably sick of hearing this by now, but generics are broken
      let pol' = case pol of
               "fixed"          -> DefFixed
               "register"       -> DefRegister
               "mustReuseInput" -> MustReuseInput
               _                -> error pol
      op <- o .: ("output" :: Text)
      return $ LDefinition vr ty' pol' op


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
                       deriving (Show)

data LDefinitionPolicy = DefFixed
                       | DefRegister
                       | MustReuseInput
                         deriving (Show)

data LAllocation = {- kind = "constantValue" -} LConstantValueAllocation
                 | {- kind = "constantIndex" -} LConstantIndexAllocation { cindex :: ConstantIndex}
                 | {- kind = "use" -} LUseAllocation { policy          :: LUsePolicy
                                                     , virtualRegister :: VirtualRegister
                                                     , registerCode    :: RegisterCode
                                                     , usedAtStart     :: Bool
                                                     }
                 | {- kind = "gpr" -} LGeneralRegAllocation { greg :: RegisterName }
                 | {- kind = "fpu" -} LFloatRegAllocation { freg :: RegisterName }
                 | {- kind = "stackSlot" -} LStackSlotAllocation { slot :: StackSlot }
                 | {- kind = "argumentSlot" -} LArgumentAllocation { lindex :: ArgumentIndex }
                   deriving (Generic, Show)

instance FromJSON LAllocation where
    parseJSON = withObject "allocation" $ \o -> do
      kindField <- o .: ("kind" :: Text)
      case kindField of
        "constantValue" -> return LConstantValueAllocation
        "constantIndex" -> do
          idx <- o .: ("index" :: Text)
          return $ LConstantIndexAllocation idx
        "use" -> do
          -- we have to do this shit because the generic
          -- for LUse is busted for some reason
          p <- o .: ("policy" :: Text)
          let policy = case (p :: Text) of
                         "any"            -> Any
                         "useRegister"    -> UseRegister
                         "useFixed"       -> UseFixed
                         "keepAlive"      -> KeepAlive
                         "recoveredInput" -> RecoveredInput
                         _                -> error "Unexpected policy type"
          vr <- o .: ("virtualRegister" :: Text)
          rc <- o .: ("registerCode" :: Text)
          uas <- o .: ("usedAtStart" :: Text)
          return $ LUseAllocation Any vr rc uas
        "gpr" -> do
          reg <- o .: ("reg" :: Text)
          return $ LGeneralRegAllocation reg
        "fpu" -> do
          reg <- o .: ("reg" :: Text)
          return $ LFloatRegAllocation reg
        "stackSlot" -> do
          slot <- o .: ("slot" :: Text)
          return $ LStackSlotAllocation slot
        "argumentSlot" -> do
          idx <- o .: ("index" :: Text)
          return $ LArgumentAllocation idx
        _               -> error kindField

data LUsePolicy = Any
                | UseRegister
                | UseFixed
                | KeepAlive
                | RecoveredInput
                  deriving (Show)


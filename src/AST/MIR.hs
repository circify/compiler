{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module AST.MIR ( module AST.MIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Control.Monad.State.Strict (join, unless, when)
import           Data.Aeson                 hiding (Object)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import           Data.Text                  hiding (concatMap, foldl, map,
                                             unwords)
import           Data.Word
import           GHC.Generics
import           Prelude                    hiding (id)

{-|

This module is an AST for IonMonkey's MIR.

-}

type LOperand = Text
type MBlockId = Word32
type LNodeId = Word32
type VirtualRegister = Word32
type RegisterCode = Word32
type ConstantIndex = Word32
type StackSlot = Word32
type ArgumentIndex = Word32
type RegisterName = Text

-- ^ Raw Firefox MIR
data MIR = MIR { blocks :: [MBlock] }
         deriving (Show, Generic)

instance FromJSON MIR where

data MBlock = MBlock { blockId     :: MBlockId
                     , kind        :: Text
                     , unreachable :: Bool
                     , marked      :: Bool
                     , preds       :: [MBlockId]
                     , succs       :: [MBlockId]
                     , resumePoint :: LNode
                     , phiNodes    :: [LNode]
                     , instrs      :: [LNode]
                     }
            deriving (Show, Generic)


instance FromJSON MBlock where
    -- parseJSON = withObject "lblock" $ \o -> do
    --   blockId <- o .: ("id" :: Text)
    --   entries <- o .: ("entryMoves" :: Text)
    --   exits <- o .: ("exitMoves" :: Text)
    --   nodes <- o .: ("nodes" :: Text)
    --   return $ LBlock blockId entries exits nodes

data LNode = LNode { id                :: LNodeId
                   , operation         :: LOperation
                   , isCall            :: Bool
                   , recoversInput     :: Bool
                   , callPreservesRegs :: [RegisterName] -- empty for all but some WASM stuff
                   , operands          :: [LAllocation]
                   , defs              :: [LDefinition]
                   , temps             :: [LDefinition]
--                   , successors        :: [LBlockId]
                   , inputMoves        :: Maybe Word32
                   , fixReuseMoves     :: Maybe Word32
                   , movesAfter        :: Maybe Word32
                   }
           deriving (Generic, Show)

isMoveGroup :: LNode -> Bool
isMoveGroup node = case operation node of
                     LMoveGroupOp{} -> True
                     _              -> False

getVirtualTemps :: LNode -> [VirtualRegister]
getVirtualTemps = map virtualReg . temps

getRealTemps :: LNode -> [RegisterName]
getRealTemps = catMaybes . map getRealLDefs . temps

getVirtualDefs :: LNode -> [VirtualRegister]
getVirtualDefs = map virtualReg . defs

getRealDefs :: LNode -> [RegisterName]
getRealDefs = catMaybes . map getRealLDefs . defs

getVirtualOperands :: LNode -> [VirtualRegister]
getVirtualOperands = catMaybes . map getVirtualAllocation . operands

getRealOperands :: LNode -> [RegisterName]
getRealOperands = catMaybes . map getRealAllocation . operands

-- Helpers: get virtual and real from LAllocations and LDefinitions

getVirtualLDefs :: LDefinition -> VirtualRegister
getVirtualLDefs = virtualReg

getRealLDefs :: LDefinition -> Maybe RegisterName
getRealLDefs = join . fmap getRealAllocation . output

getVirtualAllocation :: LAllocation -> Maybe VirtualRegister
getVirtualAllocation alloc = case alloc of
  LUseAllocation{} -> Just $ virtualRegister alloc
  _                -> Nothing

getRealAllocation :: LAllocation -> Maybe RegisterName
getRealAllocation alloc = case alloc of
  LGeneralRegAllocation{} -> Just $ greg alloc
  LFloatRegAllocation{}   -> Just $ freg alloc
  _                       -> Nothing

-- playing around

getPhi :: LNode -> Maybe (VirtualRegister, [VirtualRegister])
getPhi alloc = case operation alloc of
                 LOp "Phi" -> let inRegs = map getVirtualAllocation $ operands alloc
                                           -- crash if just?
                              in case defs alloc of
                                   [def] -> Just (getVirtualLDefs def, catMaybes inRegs)
                                   _     -> error "No out register"
                 _         -> Nothing


instance FromJSON LNode where
    -- parseJSON = withObject "node" $ \o -> do
    --   id <- o .: ("id" :: Text)
    --   op <- o .: ("operation" :: Text)
    --   iscall <- o .: ("isCall" :: Text)
    --   reci <- o .: ("recoversInput" :: Text)
    --   cpr <- o .: ("callPreservesRegs" :: Text)
    --   ops <- o .: ("operands" :: Text)
    --   defs <- o .: ("defs" :: Text)
    --   temps <- o .: ("temps" :: Text)
    --   succs <- o .: ("successors" :: Text)
    --   im <- o .: ("inputMoves" :: Text)
    --   frm <- o .: ("fixReuseMoves" :: Text)
    --   ma <- o .: ("movesAfter" :: Text)
    --   return $ LNode id op iscall reci cpr ops defs temps succs im frm ma

data LOperation = LMoveGroupOp { moves :: [LMove] }
                | LOp { code :: Text }
                deriving (Show, Generic)

instance FromJSON LOperation where
    parseJSON = withObject "operation" $ \o -> do
      code <- o .: ("code" :: Text)
      if code == "MoveGroup"
      then do
        moves <- o .: ("moves" :: Text)
        return $ LMoveGroupOp moves
        -- move group
      else return $ LOp code

data LMove = LMove { from :: LAllocation
                   , to   :: LAllocation
                   , ty   :: LDefinitionType
                   } deriving (Show, Generic)

instance FromJSON LMove where
    parseJSON = withObject "def" $ \o -> do
      from <- o .: ("from" :: Text)
      to <- o .: ("to" :: Text)
      ty <- o .: ("type" :: Text)
      return $ LMove from to (makeType ty)

data LDefinition = LDefinition { virtualReg :: VirtualRegister
                               , ty         :: LDefinitionType
                               , policy     :: LDefinitionPolicy
                               , output     :: Maybe LAllocation
                               }
                 deriving (Show, Generic)

makeDefLabels :: String -> String
makeDefLabels inStr = if inStr == "type" then "ty" else inStr

instance FromJSON LDefinition where
    parseJSON = withObject "def" $ \o -> do
      vr <- o .: ("virtualRegister" :: Text)
      ty <- o .: ("type" :: Text)
      pol <- o .: ("policy" :: Text)
      out <- o .: ("output" :: Text)
      return $ LDefinition vr (makeType ty) (makePolicy pol) out

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

makeType ty = case ty of
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
        _              -> error $ unwords ["Unexpected LDefinitionType", ty]

data LDefinitionPolicy = DefFixed
                       | DefRegister
                       | MustReuseInput
                         deriving (Show)

makePolicy pol = case pol of
  "fixed"          -> DefFixed
  "register"       -> DefRegister
  "mustReuseInput" -> MustReuseInput
  _                -> error $ unwords ["Unexpected definition policy", pol]

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

-- Functions for interacting with the AST

isLoad :: LNode -> Bool
isLoad node = undefined --opName node `elem` loadList

loadList :: [LOperand]
loadList = [ "LoadElementV"
           , "LoadElementHole"
           , "LoadElementT"
           , "LoadElementFromStateV"
           , "LoadUnboxedScalar"
           , "LoadTypedArrayElementHole"
           , "LoadFixedSlotV"
           , "LoadFixedSlotT"
           , "LoadFixedSlotAndUnbox"
           , "LoadSlotV"
           , "LoadSlotT"
           , "WasmLoadTls"
           , "WasmLoad"
           , "WasmLoadI64"
           , "AsmJSLoadHeap"
           , "WasmLoadSlot"
           , "WasmLoadSlotI64"
           ]

isStore :: LNode -> Bool
isStore node = undefined --opName node `elem` storeList

storeList :: [LOperand]
storeList = [ "StoreElementV"
            , "StoreElementT"
            , "StoreElementHoleV"
            , "StoreElementHoleT"
            , "FallibleStoreElementV"
            , "FallibleStoreElementT"
            , "StoreUnboxedScalar"
            , "StoreTypedArrayElementHole"
            , "StoreFixedSlotV"
            , "StoreFixedSlotT"
            , "StoreSlotV"
            , "StoreSlotT"
            , "WasmStore"
            , "WasmStoreI64"
            , "AsmJSStoreHeap"
            , "WasmStoreSlot"
            , "WasmStoreSlotI64"
            , "WasmStoreRef"
            ]

isFnCall :: LNode -> Bool
isFnCall node = undefined --opName node `elem` callList

callList :: [LOperand]
callList = [ "CallGeneric"
           , "CallKnown"
           , "CallNative"
           , "CallDOMNative"
           , "CallDirectEval"
           , "CallGetIntrinsicValue"
           , "CallBindVar"
           , "CallGetProperty"
           , "CallGetElement"
           , "CallSetElement"
           , "CallInitElementArray"
           , "CallSetProperty"
           , "CallDeleteProperty"
           , "CallDeleteElement"
           , "WasmCall"
           , "IonToWasmCall"
           , "IonToWasmCallV"
           ]

isCond :: LNode -> Bool
isCond node = undefined --opName node `elem` condList

condList :: [LOperand]
condList = [ "TestIAndBranch"
           , "TestI64AndBranch"
           , "TestDAndBranch"
           , "TestFAndBranch"
           , "TestOAndBranch"
           , "TestVAndBranch"
           , "CompareI64AndBranch"
           , "CompareAndBranch"
           , "CompareDAndBranch"
           , "CompareFAndBranch"
           , "CompareBAndBranch"
           , "CompareBitwiseAndBranch"
           , "BitAndAndBranch"
           , "IsNullOrLikeUndefinedAndBranchV"
           , "IsNullOrLikeUndefinedAndBranchT"
           , "IsNoIterAndBranch"
           , "IsObjectAndBranch"
           , "IsNullOrUndefinedAndBranch"
           ]

isReturn :: LNode -> Bool
isReturn node = undefined -- opName node `elem` returnList

returnList :: [LOperand]
returnList = [ "ReturnFromCtor"
             , "Return"
             , "OsrReturnValue"
             , "WasmReturn"
             , "WasmReturnI64"
             , "WasmReturnVoid"
             , "CheckReturn"
             ]




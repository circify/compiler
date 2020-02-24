module AST.LIR ( module AST.LIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Data.Word

{-|

This module is an AST for IonMonkey's LIR.
It works with the LIR opcodes presented here (and other arch-specific files):
https://searchfox.org/mozilla-central/source/__GENERATED__/__macosx64__/js/src/jit/LOpcodes.h#10

There are no semantics for this IR, so we go on the implementation at:
https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#6427

-}

--
-- Basic types
--

data Ty = Undefined
        -- | Null
        -- | Boolean
        -- | Int32
        -- | Int64
        -- | Double
        -- | Float32
        -- | String
        -- | Symbol
        -- | BigInt
        -- | Object
        -- | MagicOptimizedArguments
        -- | MagicOptimizedOut
        -- | MagicHole
        -- | MagicIsConstructing
        -- | MagicUninializedLexical
        -- | Value
        -- | ObjectOrNull
        -- | None
        -- | SlotsTy
        -- | ElementsTy
        -- | PointerTy

-- | Figure out what these actually look like in the serialized LIR
-- before and after register allocation
-- https://searchfox.org/mozilla-central/source/js/src/jit/Registers.h#32
data Register = Register

-- | https://searchfox.org/mozilla-central/source/js/src/jit/MIR.h#8292
type Slot   = Word

-- | https://searchfox.org/mozilla-central/source/js/src/jit/RegisterSets.h#115
data Value  = Value { valueType    :: Register
                    , valuePayload :: Register
                    }


-- | HAVE NOT FIGURED OUT YET
data AnyReg     = AnyReg
data AllocValue = AllocValue
data LAlloc = LAlloc

data Address = Address

data LDefinition = LDefinition
data LAllocation = LAllocation

--data Call =


-- | Memory operations in LIR
data Mem = StoreFixedSlotV { object       :: LAllocation
                           , value        :: LAllocation
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10951
         -- Object: ToRegister(ins->getOperand(0)) where getOperand returns an LAllocation
         -- Value: const ValueOperand value = ToValue(ins, LStoreFixedSlotV::Value)
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotV { object :: LAllocation
                          , slot   :: Slot
                          , result :: LAllocation
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10900
         -- Object: ToRegister(ins->getOperand(0)), see rationale above
         -- Slot: size_t slot = ins->mir()->slot()
         -- Result: ValueOperand result = ToOutValue(ins)
         | StoreFixedSlotT { object       :: LAllocation
                           , value        :: LAllocation
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10965
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- ALLOC VALUE UNCLEAR
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotT { object  :: LAllocation
                          , slot    :: Slot
                          , result' :: LDefinition
                          , ty      :: Ty
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10908
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Slot: size_t slot = ins->mir()->slot()
         -- Result: https://searchfox.org/mozilla-central/source/js/src/jit/LIR.h#955
         -- Ty: MIRType type = ins->mir()->type()
         | LoadFixedSlotAndUnbox { input    :: LAllocation
                                 , slot     :: Slot
                                 , result'  :: LDefinition
                                 , ty       :: Ty
                                 , fallible :: Bool
                                 }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10918
         -- Input: const Register input = ToRegister(ins->getOperand(0)
         -- Slot: slot = mir->slot()
         -- Result: AnyRegister result = ToAnyRegister(ins->output()) where:
         -- https://searchfox.org/mozilla-central/source/js/src/jit/LIR.h#986
         -- Type: MIRType type = mir->type()
         -- Falliable: if (mir->fallible())
         | StoreSlotV { base         :: LAllocation
                      , slot         :: Slot
                      , value'       :: (LAllocation, LAllocation)
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#4020
         -- Base: Register base = ToRegister(lir->slots())
         -- Slot: lir->mir()->slot()
         -- Value: const ValueOperand value = ToValue(lir, LStoreSlotV::Value) where
         -- https://searchfox.org/mozilla-central/source/js/src/jit/arm/CodeGenerator-arm.cpp#1317
         -- NeedsBarrier: if (lir->mir()->needsBarrier())
         | LoadSlotV { dest' :: (LAllocation, LAllocation)
                     , base  :: LAllocation
                     , slot  :: Slot
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3985
         -- Dest: ValueOperand dest = ToOutValue(lir)
         -- https://searchfox.org/mozilla-central/source/js/src/jit/shared/CodeGenerator-shared-inl.h#162
         -- Base: Register base = ToRegister(lir->input()) where
         -- https://searchfox.org/mozilla-central/source/js/src/jit/LIR.h#982
         -- Slot: lir->mir()->slot()
         | StoreSlotT { base         :: LAllocation
                      , avalue       :: LAllocation
                      , slot         :: Slot
                      , ty           :: Ty
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3993
         -- Base: Register base = ToRegister(lir->slots()) where lir's slots is LAllocation
         -- Value: lir->value()
         -- Slot: lir->mir()->slot()
         -- Ty: MIRType valueType = lir->mir()->value()->type()
         -- NeedsBarrier: (lir->mir()->needsBarrier())
         | LoadSlotT { base    :: LAllocation
                     , slot    :: Slot
                     , result' :: LDefinition
                     , ty      :: Ty
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3977
         -- Base: Register base = ToRegister(lir->slots())
         -- Slot: lir->mir()->slot()
         -- Output: AnyRegister result = ToAnyRegister(lir->output()) where
         -- https://searchfox.org/mozilla-central/source/js/src/jit/shared/LIR-shared.h#4975
         -- Type: lir->mir()->type()
         | StoreElementV { value          :: LAllocation
                         , elements       :: LAllocation
                         , index          :: LAllocation
                         , offsetAdj      :: Int
                         , needsBarrier   :: Bool
                         , needsHoleCheck :: Bool
                         }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9505
         -- Value: const ValueOperand value = ToValue(lir, LStoreElementV::Value)
         -- https://searchfox.org/mozilla-central/source/js/src/jit/x64/CodeGenerator-x64.cpp#27
         -- Elements: Register elements = ToRegister(lir->elements())
         -- https://searchfox.org/mozilla-central/source/js/src/jit/shared/LIR-shared.h#4076
         -- Index: const LAllocation* index = lir->index()
         -- OffsetAdjustement: lir->mir()->offsetAdjustment() where
         --https://searchfox.org/mozilla-central/source/js/src/jit/MIR.h#7676
         -- NeedsBarrier: lir->mir()->needsBarrier()
         -- NeedsHoleCheck: lir->mir()->needsHoleCheck()
         | LoadElementV { elements       :: LAllocation
                        , out'           :: (LAllocation, LAllocation)
                        , index          :: LAllocation
                        , offsetAdj      :: Int
                        , needsHoleCheck :: Bool
                        }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11506
         -- Elements: Register elements = ToRegister(load->elements())
         -- Out: const ValueOperand out = ToOutValue(load)
         -- Index: load->index()
         -- OffsetAdjustment: load->mir()->offsetAdjustment()
         -- NeedsHolecheck: load->mir()->needsHoleCheck()
         | StoreElementT { value          :: LAllocation
                         , ty             :: Ty
                         , elements       :: LAllocation
                         , elemTy         :: Ty
                         , index          :: LAllocation
                         , offsetAdj      :: Int
                         , needsBarrier   :: Bool
                         , needsHoleCheck :: Bool
                         }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9487
         -- Value: store->value()
         -- Type: store->mir()->value()->type()
         -- Elements: Register elements = ToRegister(store->elements())
         -- Element Type: store->mir()->elementType()
         -- Index: const LAllocation* index = store->index()
         -- OffsetAdjustment: store->mir()->offsetAdjustment()
         -- NeedsBarrier: store->mir()->needsBarrier()
         -- NeedsHoleCheck: store->mir()->needsHoleCheck()
         | StoreElementHoleV { elements       :: LAllocation
                             , index          :: LAllocation
                             , value          :: LAllocation
                             , spectreTemp    :: LDefinition
                             , needsBarrier   :: Bool
                             , needsHoleCheck :: Bool
                             , strict         :: Bool
                             }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9576
         -- Elements: Register elements = ToRegister(lir->elements())
         -- Index: Register index = ToRegister(lir->index())
         -- Value: const ValueOperand value = ToValue(lir, T::Value)
         -- SpectreTemp: ToTempRegisterOrInvalid(lir->spectreTemp()) where
         -- https://searchfox.org/mozilla-central/source/js/src/jit/shared/LIR-shared.h#4179

--         | StoreElementHoleT

--          | StoreElementHoleV Value Elements Index SpectreTemp NeedsBarrier
--          | StoreElementHoleT TypedValue Elements Index SpectreTemp NeedsBarrier
--          | LoadElementHole Elements Index InitLength Result NeedsHoleCheck
--          | FallibleStoreElementV Value Elements Index SpectreTemp NeedsBarrier
--          | FallibleStoreElementT TypedValue Elements Index SpectreTemp NeedsBarrier
-- --         | LoadElementFromStateV Index Tmp
--          | StoreUnboxedScalar Value Elements WriteType
--          | LoadUnboxedScalar Elements Index Temp ReadType
--          | StoreTypedArrayElementHole Elements Value ArrayType Index Length
--         | LoadTypedArrayElementHole Object




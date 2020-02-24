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



-- | Memory operations in LIR
data Mem = StoreFixedSlotV { object       :: Register
                           , value        :: Value
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10951
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Value: const ValueOperand value = ToValue(ins, LStoreFixedSlotV::Value)
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotV { object :: Register
                          , slot   :: Slot
                          , result :: Value
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10900
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Slot: size_t slot = ins->mir()->slot()
         -- Result: ValueOperand result = ToOutValue(ins)
         | StoreFixedSlotT { object       :: Register
                           , avalue       :: AllocValue
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10965
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- ALLOC VALUE UNCLEAR
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotT { object  :: Register
                          , slot    :: Slot
                          , aresult :: AnyReg
                          , ty      :: Ty
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10908
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Slot: size_t slot = ins->mir()->slot()
         -- Result: ?????????
         -- Ty: MIRType type = ins->mir()->type()
         | LoadFixedSlotAndUnbox { input    :: Register
                                 , slot     :: Slot
                                 , aresult  :: AnyReg
                                 , ty       :: Ty
                                 , fallible :: Bool
                                 }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10918
         -- Input: const Register input = ToRegister(ins->getOperand(0)
         -- Slot: slot = mir->slot()
         -- Result: AnyRegister result = ToAnyRegister(ins->output())
         -- Type: MIRType type = mir->type()
         -- Falliable: if (mir->fallible())
         | StoreSlotV { base         :: Register
                      , slot         :: Slot
                      , value        :: Value
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#4020
         -- Base: Register base = ToRegister(lir->slots())
         -- Slot: lir->mir()->slot()
         -- Value: const ValueOperand value = ToValue(lir, LStoreSlotV::Value)
         -- NeedsBarrier: if (lir->mir()->needsBarrier())
         | LoadSlotV { dest :: Value
                     , base :: Register
                     , slot :: Slot
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3985
         -- Dest: ValueOperand dest = ToOutValue(lir)
         -- Base: Register base = ToRegister(lir->input())
         -- Slot: lir->mir()->slot()
         | StoreSlotT { base         :: Register
                      , avalue       :: AllocValue
                      , slot         :: Slot
                      , ty           :: Ty
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3993
         -- Base: Register base = ToRegister(lir->slots())
         -- Value: lir->value()
         -- Slot: lir->mir()->slot()
         -- Ty: MIRType valueType = lir->mir()->value()->type()
         -- NeedsBarrier: (lir->mir()->needsBarrier())
         | LoadSlotT { base    :: Register
                     , slot    :: Slot
                     , aresult :: AnyReg
                     , ty      :: Ty
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3977
         -- Base: Register base = ToRegister(lir->slots())
         -- Slot: lir->mir()->slot()
         -- Output: AnyRegister result = ToAnyRegister(lir->output())
         -- Type: lir->mir()->type()
         | StoreElementV { value          :: Value
                         , elements       :: Register
                         , index          :: AllocValue
                         , offsetAdj      :: Int
                         , needsBarrier   :: Bool
                         , needsHoleCheck :: Bool
                         }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9505
         -- Value: const ValueOperand value = ToValue(lir, LStoreElementV::Value)
         -- Elements: Register elements = ToRegister(lir->elements())
         -- Index: const LAllocation* index = lir->index()
         -- OffsetAdjustement: lir->mir()->offsetAdjustment()
         -- NeedsBarrier: lir->mir()->needsBarrier()
         -- NeedsHoleCheck: lir->mir()->needsHoleCheck()
         | LoadElementV { elements       :: Register
                        , out            :: Value
                        , index          :: AllocValue
                        , offsetAdj      :: Int
                        , needsHoleCheck :: Bool
                        }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11506
         -- Elements: Register elements = ToRegister(load->elements())
         -- Out: const ValueOperand out = ToOutValue(load)
         -- Index: load->index()
         -- OffsetAdjustment: load->mir()->offsetAdjustment()
         -- NeedsHolecheck: load->mir()->needsHoleCheck()
         | StoreElementT { avalue         :: AllocValue
                         , ty             :: Ty
                         , elements       :: Register
                         , elemTy         :: Ty
                         , index          :: AllocValue
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
         | StoreElementHoleV { elements       :: Register
--                             , index          :: Register
                             , value          :: Value
                             , spectreTemp    :: Register
                             , needsBarrier   :: Bool
                             , needsHoleCheck :: Bool
                             , strict         :: Bool
                             }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9576
         --
         | StoreElementHoleT

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




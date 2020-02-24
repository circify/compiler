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

--
-- Typedefs. These are reflected in the source
--

-- | Results are also values:
-- https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10903
-- We get results from their instructions here:
-- https://searchfox.org/mozilla-central/source/js/src/jit/shared/CodeGenerator-shared-inl.h#162
type Result = Value

-- | 'Object' is an example of a register:
-- https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10952
type Object = Register

-- | https://searchfox.org/mozilla-central/source/js/src/jit/MIR.h#8297
type NeedsBarrier = Bool

-- | https://searchfox.org/mozilla-central/source/js/src/jit/MIR.h#7675
type NeedsHoleCheck = Bool

-- | Memory operations in LIR
data Mem = StoreFixedSlotV Object Value Slot NeedsBarrier
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10951
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Value: const ValueOperand value = ToValue(ins, LStoreFixedSlotV::Value)
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotV Object Slot Result
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10900
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         -- Slot: size_t slot = ins->mir()->slot()
         -- Result: ValueOperand result = ToOutValue(ins)
         | StoreFixedSlotT Object           Slot NeedsBarrier
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10965
         -- Object: const Register obj = ToRegister(ins->getOperand(0))
         --
         -- Slot: size_t slot = ins->mir()->slot()
         -- NeedsBarrier: if (ins->mir()->needsBarrier())
         | LoadFixedSlotT Object Slot
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10908
         --

--          | StoreFixedSlotT Object TypedValue Slot NeedsBarrier
--          | LoadFixedSlotT Object Slot TypedResult
--          | LoadFixedSlotAndUnbox Input Slot TypedResult
--          | StoreSlotV Base Value Slot NeedsBarrier
--          | StoreSlotT Base TypedValue Slot NeedsBarrier
--          | LoadSlotV Base Slot Result
--          | LoadSlotT Base Slot TypedResult
--          | StoreElementV Value Elements Index NeedsBarrier NeedsHoleCheck
--          | StoreElementT TypedValue Elements Index NeedsBarrier NeedsHoleCheck
--          | LoadElementV Elements Index Result NeedsHoleCheck
--          | LoadElementT Elements Index TypedResult NeedsHoleCheck
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




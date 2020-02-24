module IR.LIR where
import           AST.LIR

-- | https://searchfox.org/mozilla-central/source/js/src/jit/arm/MacroAssembler-arm.cpp#2988
storeValue :: Value -> Address
storeValue = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/MacroAssembler-inl.h#859
storeObjectOrNull = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/MacroAssembler.h#2646
storeConstantOrRegister = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/arm/MacroAssembler-arm.cpp#4637
-- https://searchfox.org/mozilla-central/source/js/src/jit/x64/MacroAssembler-x64.cpp#548
storeUnboxedValue = undefined

-- | https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11879
storeToTypedArray = undefined



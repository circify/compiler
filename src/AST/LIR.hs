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

type Slot   = Word
data MDefintion = MDefinition
data LDefinition = LDefinition
data LAllocation = LAllocation

data Variable = Variable

data Op = Op [Variable]

data Return = ReturnFromCtor { retValue  :: LAllocation -- not sure
                             , retObj    :: LAllocation
                             , retOutput :: LAllocation
                             }
            -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#7130
            | OsrReturnValue { retFrame :: LAllocation
                             , retOut   :: (LAllocation, LAllocation)
                             }
            -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3833
            | Return
            -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3758

data Call = CallKnown { calleeReg :: LAllocation
                      , objReg    :: LDefinition
                      , argSlot   :: Int
--                      , target :: Wrapped function
                      } -- main
          | CallNative -- main
          | CallGetIntrinsicValue
          | CallBindVar
          | CallGetProperty
          | CallGetElement
          | CallSetElement
          | CallInitElementArray
          | CallSetProperty
          | CallDeleteProperty
          | CallDeleteElement
          | NewCallObject
          | CallGeneric
          | CallDOMNative
          | CallDirectEval

-- | Memory operations in LIR
--
-- Missing LoadElementFromStateV, which is insane:
-- https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11746
--
data Mem = StoreFixedSlotV { object       :: LAllocation
                           , value        :: LAllocation
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10951
         | LoadFixedSlotV { object :: LAllocation
                          , slot   :: Slot
                          , result :: LAllocation
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10900
         | StoreFixedSlotT { object       :: LAllocation
                           , value        :: LAllocation
                           , slot         :: Slot
                           , needsBarrier :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10965
         | LoadFixedSlotT { object  :: LAllocation
                          , slot    :: Slot
                          , result' :: LDefinition
                          , ty      :: Ty
                          }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10908
         | LoadFixedSlotAndUnbox { input    :: LAllocation
                                 , slot     :: Slot
                                 , result'  :: LDefinition
                                 , ty       :: Ty
                                 , fallible :: Bool
                                 }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#10918
         | StoreSlotV { base         :: LAllocation
                      , slot         :: Slot
                      , value'       :: (LAllocation, LAllocation)
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#4020
         | LoadSlotV { dest' :: (LAllocation, LAllocation)
                     , base  :: LAllocation
                     , slot  :: Slot
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3985
         | StoreSlotT { base         :: LAllocation
                      , avalue       :: LAllocation
                      , slot         :: Slot
                      , ty           :: Ty
                      , needsBarrier :: Bool
                      }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3993
         | LoadSlotT { base    :: LAllocation
                     , slot    :: Slot
                     , result' :: LDefinition
                     , ty      :: Ty
                     }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#3977
         | StoreElementV { value          :: LAllocation
                         , elements       :: LAllocation
                         , index          :: LAllocation
                         , offsetAdj      :: Int
                         , needsBarrier   :: Bool
                         , needsHoleCheck :: Bool
                         }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9505
         | LoadElementV { elements       :: LAllocation
                        , out'           :: (LAllocation, LAllocation)
                        , index          :: LAllocation
                        , offsetAdj      :: Int
                        , needsHoleCheck :: Bool
                        }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11506
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
         | LoadElementT { output'        :: LDefinition
                        , ty             :: Ty
                        , loadDoubles    :: Bool
                        , needsHoleCheck :: Bool
                        }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11473
         | StoreElementHoleV { elements       :: LAllocation
                             , index          :: LAllocation
                             , value          :: LAllocation
                             , spectreTemp    :: LDefinition
                             , needsBarrier   :: Bool
                             , needsHoleCheck :: Bool
                             , strict         :: Bool
                             }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9576
         | StoreElementHoleT { elements     :: LAllocation
                             , elementTy    :: Ty
                             , index        :: LAllocation
                             , value        :: LAllocation
                             , ty           :: Ty
                             , spectreTemp  :: LDefinition
                             , needsBarrier :: Bool
                             , strict       :: Bool
                             }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9531
         | LoadElementHole { elements              :: LAllocation
                           , index                 :: LAllocation
                           , initLength            :: LAllocation
                           , out'                  :: (LAllocation, LAllocation)
                           , needsHoleCheck        :: Bool
                           , needsNegativeIntCheck :: Bool
                           }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11528
         | FallibleStoreElementV { elements       :: LAllocation
                                 , index          :: LAllocation
                                 , value'         :: (LAllocation, LAllocation) -- ?
                                 , spectreTemp    :: LDefinition
                                 , needsHoleCheck :: Bool
                                 , strict         :: Bool
                                 }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9576
         | FallibleStoreElementT { elements       :: LAllocation
                                 , elementTy      :: Ty
                                 , index          :: LAllocation
                                 , value          :: LAllocation
                                 , ty             :: Ty
                                 , spectreTemp    :: LDefinition
                                 , needsBarrier   :: Bool
                                 , needsHoleCheck :: Bool
                                 , strict         :: Bool
                                 }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#9531
         | StoreUnboxedScalar { elements  :: LAllocation
                              , value     :: LAllocation
                              , index     :: LAllocation
                              , writeTy   :: Ty
                              , storageTy :: Ty
                              , offsetAdj :: Int
                              }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#
         | LoadUnboxedScalar { elements      :: LAllocation
                             , index         :: LAllocation
                             , temp          :: LAllocation
                             , out           :: LDefinition
                             , readTy        :: Ty
                             , cannonDoubles :: Bool
                             }
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11572
         | StoreTypedArrayElementHole
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11914
         | LoadTypedArrayElementHole
         -- ^ https://searchfox.org/mozilla-central/source/js/src/jit/CodeGenerator.cpp#11602



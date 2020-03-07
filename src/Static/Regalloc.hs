module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Static.Kildall

{-|

Register allocation translation validation as presented in:
https://xavierleroy.org/publi/validation-regalloc.pdf

-}

addAssignment :: VirtualRegister
              -> RegisterName
              -> M.Map RegisterName VirtualRegister
              -> M.Map RegisterName VirtualRegister
addAssignment vr rr regs = case M.lookup rr regs of
                             Just vr' | vr == vr' -> regs
                             Just{}   -> error "Already assigned virutal reg"
                             Nothing  -> M.insert rr vr regs

-- | Per program point association between virtual registers and real registers
-- (page 8 of the paper)
data Locs = Locs (S.Set (VirtualRegister, RegisterName)) -- No bug, just locations
          | Broken -- Found a bug
          | Star -- The start state for the whole store

instance Checkable Locs where
    meet (Locs locs1) (Locs locs2) = Locs $ S.union locs1 locs2
    meet Broken _                  = Broken
    meet _ Broken                  = Broken
    meet locs1 Star                = locs1
    meet Star locs2                = locs2

    transfer = transferFn


transferFn :: [LIR] -- ^ Before regalloc
           -> WorkNode Locs
           -> WorkNode Locs
transferFn [before,after] locs = undefined

-- transfer :: WorkNode Locs
--          -> WorkNode Locs
-- transfer before after info
--   | isLoad after   = undefined
--   | isStore after  = undefined
--   | isFnCall after = undefined
--   | isCond after   = undefined
--   | isReturn after = undefined
--   | otherwise      = undefined


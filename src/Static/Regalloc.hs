module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import qualified Data.Set       as S
import           Static.Kildall

{-|

Register allocation translation validation as presented in:
https://xavierleroy.org/publi/validation-regalloc.pdf

-}

-- | Per program point association between virtual registers and real registers
-- (page 8 of the paper)
data Locs = Locs (S.Set (VirtualRegister, RegisterName))
          | Broken

-- | Transfer function for the analysis
-- (page 10)
transfer :: LNode -- ^ Before node
         -> LNode -- ^ After node
         -> Locs -- ^ Location information before transfer
         -> Locs -- ^ Location information after transfer
transfer _ _ Broken        = Broken
transfer before after info
  | isLoad after   = undefined
  | isStore after  = undefined
  | isFnCall after = undefined
  | isCond after   = undefined
  | isReturn after = undefined
  | otherwise      = undefined


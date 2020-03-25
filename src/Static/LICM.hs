module Static.LICM where
import           AST.MIR
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Static.KildallMIR

type SSAName = (Int, Int)

data Deps = Start
          | DepMap { depmap :: M.Map SSAName (S.Set SSAName) }
          deriving (Eq)

transfer' :: [MIR] -> WorkNode Deps -> IO (WorkNode Deps)
transfer' = undefined

meet' :: Deps
      -> Deps
      -> IO Deps
meet' = undefined

instance Checkable Deps where
    meet = meet'
    transfer = transfer'

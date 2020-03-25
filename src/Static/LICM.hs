module Static.LICM where
import           AST.MIR
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Static.KildallMIR

type SSAName = (Int, Int)

data Deps = Start
          | DepMap { depmap :: M.Map SSAName (S.Set SSAName) }


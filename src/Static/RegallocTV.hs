module Static.RegallocTV where
import           AST.LIR
import           AST.Regalloc
import qualified Data.Map     as M
import           Data.Text

data LocationSet = LocationSet

-- getVirtualRegisters :: LNode -> M.Map Text [Text] -> M.Map Text [Text]
-- getVirtualRegisters node regmap =
--   let regmap'  = M.insert "preservesRegs" (callPreservesRegs node) regmap
--       regmap'' = M.insert

--   where getRegsDef = virtualRegister




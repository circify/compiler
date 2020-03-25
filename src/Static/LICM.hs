module Static.LICM where
import           AST.MIR
import           Control.Monad.State.Strict (unless)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Static.KildallMIR

data Deps = Start
          | DepMap { depmap :: M.Map NodeId (S.Set NodeId) }
          deriving (Eq)

transfer' :: [MIR] -> WorkNode Deps -> IO (WorkNode Deps)
transfer' [program] node = do
  let blockId = fst $ workNode node
      nodeId  = snd $ workNode node
      name    = (blockId, nodeId)
  node' <- lookupNode program node
  let ops = map (\op -> (topBlockId op, topId op)) $ operands node'
      state' = DepMap $ case nodeState node of
                 Start     -> M.fromList [(name, S.fromList ops)]
                 DepMap ds -> M.insert name (S.fromList ops) ds
  return $ WorkNode (workNode node) state'
transfer' _ _            = error "Unexpected format for MIR"

meet' :: Deps
      -> Deps
      -> IO Deps
meet' = undefined

lookupNode :: MIR
           -> WorkNode a
           -> IO MNode
lookupNode mir (WorkNode (bid, nid) _) = do
  let bs     = makeBlockMap $ blocks mir
  unless (M.member bid bs) $
    error $ unwords [show bid, "not in", show bs]
  let block  = bs M.! bid
      ns     = makeNodeMap $ instrs block
  unless (M.member nid ns) $
    error $ unwords [show nid, "not in", show ns]
  let ret = ns M.! nid
  return ret

instance Checkable Deps where
    meet = meet'
    transfer = transfer'

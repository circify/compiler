module Static.LICM where
import           AST.MIR
import           Control.Monad.State.Strict (unless)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Prelude                    hiding (id)
import           Static.KildallMIR

data Deps = Start
          | DepMap { depmap :: M.Map NodeId (S.Set NodeId) }
          deriving (Show)

instance Eq Deps where
    Start == _ = False
    _ == Start = False
    (DepMap m) == (DepMap n) = m == n

isStart :: Deps -> Bool
isStart Start = True
isStart _     = False

isMap :: Deps -> Bool
isMap DepMap{} = True
isMap _        = False

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
meet' r1 r2
    | isStart r1 && isStart r2 = return $ DepMap M.empty
    | isStart r1 = return r2
    | isStart r2 = return r1
    | otherwise = return $ DepMap $ M.unionWith S.union (depmap r1) (depmap r2)
    | otherwise = error ""

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

initList :: MIR -> [WorkNode Deps]
initList mir =
  concatMap (\b -> map (\n -> WorkNode (blockId b, id n) Start) $ instrs b) $ blocks mir

initState :: MIR -> Store Deps
initState mir =
  let nodes = map workNode $ initList mir
  in Store $ foldl (\m n -> M.insert n Start m) M.empty nodes

instance Checkable Deps where
    meet = meet'
    transfer = transfer'

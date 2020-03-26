module Static.LICM where
import           AST.MIR
import           Control.Monad.State.Strict (forM_, unless)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Prelude                    hiding (id)
import           Static.KildallMIR

-- x = 5      after: { x }
-- y = x + 12 after: { y, x }
-- z = foo(y) after: { z, y, x }
-- y = foo(z) after: { z, y, x }

data Defs = Start
          | Defs { defs :: S.Set NodeId }
          deriving (Show)

instance Eq Defs where
    Start == _ = False
    _ == Start = False
    (Defs s) == (Defs d) = s == d

isStart :: Defs -> Bool
isStart Start = True
isStart _     = False

isMap :: Defs -> Bool
isMap Defs{} = True
isMap _      = False

check :: MIR -> Store Defs -> IO ()
check mir defs = do
  forM_ (M.toList $ storeMap defs) $ \(nodeId, ds) -> do
    node <- lookupNode mir (WorkNode nodeId S.empty)
    let useds = map (\op -> (topBlockId op, topId op)) $ operands node
    forM_ useds $ \use ->
      case ds of
        Start  -> error "Uninitialized analysis set"
        Defs d -> unless (S.member use d) $ error $ unwords [ "Expected"
                                                            , show use
                                                            , "in"
                                                            , show d
                                                            , "at"
                                                            , show nodeId
                                                            ]

transfer' :: [MIR] -> WorkNode Defs -> IO (WorkNode Defs)
transfer' [program] node = do
  let name  = workNode node
      start = case nodeState node of
                Start   -> Defs $ S.singleton name
                Defs ds -> Defs $ S.insert name ds
  return $ WorkNode name start
transfer' _ _            = error "Unexpected format for MIR"

meet' :: Defs
      -> Defs
      -> IO Defs
meet' r1 r2
    | isStart r1 && isStart r2 = return $ Defs S.empty
    | isStart r1 = return r2
    | isStart r2 = return r1
    | otherwise = return $ Defs $ S.union (defs r1) (defs r2)
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

initList :: MIR -> [WorkNode Defs]
initList mir =
  concatMap (\b -> map (\n -> WorkNode (blockId b, id n) Start) $ instrs b) $ blocks mir

initState :: MIR -> Store Defs
initState mir =
  let nodes = map workNode $ initList mir
  in Store $ foldl (\m n -> M.insert n Start m) M.empty nodes

instance Checkable Defs where
    meet = meet'
    transfer = transfer'

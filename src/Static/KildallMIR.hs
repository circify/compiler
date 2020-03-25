module Static.KildallMIR where
import           AST.MIR
import           Control.Monad.State.Strict (forM, forM_, unless, when)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Prelude                    hiding (id)

-- | A unique identifier for an MIR node in the program
type NodeId = (MBlockId, MNodeId)

-- | A pair of (nodeId, analysis information)
data WorkNode a = WorkNode { workNode  :: NodeId
                           , nodeState :: a
                           }
                  deriving (Eq, Ord, Show)


-- | A store of information about the analysis state of nodes in the program
data Store a = Store { storeMap :: (M.Map NodeId a) }
             deriving (Show, Eq)


getSuccessors :: (Show a) => WorkNode a -> [MIR] -> IO [NodeId]
getSuccessors _ []    = error "Cannot examine empty program"
getSuccessors node [program] = do
  let blockid = fst wn
      blocks' = makeBlockMap $ blocks program
  unless (M.member blockid blocks') $
    error $ unwords ["Block id"
                    , show blockid
                    , "outside of range"
                    , show blocks'
                    ]
  let block  = blocks' M.! blockid
      nodes' = makeNodeMap $ instrs block
      nodeid = snd wn
  unless (M.member nodeid nodes') $
    error $ unwords ["Node id"
                    , show nodeid
                    , "outside of range"
                    , show nodes'
                    ]
  let node' = nodes' M.! nodeid
      ns      = map id $ instrs block
      idx     = fromJust $ L.elemIndex nodeid ns
      nextIdx = idx + 1
  if nextIdx >= length ns
  -- we need to look in the next block for the successor node
  then do
    let fin = map (\b -> let nextBlock = blocks' M.! b
                         in case instrs nextBlock of
                              []   -> error "Empty next block"
                              n:ns -> (b, id n)
                  ) $ succs block
    return fin
  -- we can look in the current block for the successor node
  else return $ [(fst wn, ns !! nextIdx)]
  where wn = workNode node

-- Functions for interacting with the store

infoAt :: (Show a) => WorkNode a -> Store a -> a
infoAt node (Store store) = case M.lookup (workNode node) store of
                      Just item -> item
                      Nothing -> error $ unwords [ "Could not find store info for"
                                                 , show $ workNode node
                                                 , "in"
                                                 , "\n"
                                                 , show store
                                                 ]

updateStore :: WorkNode a -> a -> Store a -> Store a
updateStore node item (Store store) = Store $ M.insert (workNode node) item store

-- | We have to be able to combine two peices of analysis information
-- in order to check the program using Kilall's algorithm
-- We also need a transfer function that "propagates information thru an expression"
class Checkable a where
    meet :: a -> a -> IO a
    transfer :: [MIR] -> WorkNode a -> IO (WorkNode a)

-- | http://www.ccs.neu.edu/home/types/resources/notes/kildall/kildall.pdf
-- Kildall's algorithm for computing program information to a fixed point
kildall :: (Checkable a, Eq a, Show a)
        => [WorkNode a] -- ^ Worklist
        -> Store a -- ^ Store of analysis information
        -> MIR
        -> IO (Store a)
kildall [] store _ = return store
kildall (elem:rest) store mir = do
  let incomingState = nodeState elem
      currentState = infoAt elem store
  newState <- meet incomingState currentState
  if newState == currentState
  then kildall rest store mir
  else do
    succs <- getSuccessors elem [mir]
    let newStore = updateStore elem newState store
    transferredState <- transfer [mir] $ WorkNode (workNode elem) newState
    next <- forM succs $ \e -> return $ WorkNode e (nodeState transferredState)
    kildall (rest++next) newStore mir


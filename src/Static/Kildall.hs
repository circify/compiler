module Static.Kildall where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (forM, forM_, unless, when)
import qualified Data.List     as L (elem, elemIndex)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)
import           Prelude       hiding (id)

{-|

This module performs Kildall's algorithm for computing program information
to a fixed point.

-}

-- | A unique identifier for an LIR node in the program
type NodeId = (LBlockId, LNodeId)

-- | A pair of (nodeId, analysis information)
data WorkNode a = WorkNode { workNode  :: NodeId
                           , nodeState :: a
                           }
                  deriving (Eq, Ord, Show)


-- | A store of information about the analysis state of nodes in the program
data Store a = Store { storeMap :: (M.Map NodeId a) }
             deriving (Show)

-- Functions for interacting with successors

getSuccessors :: (Show a) => WorkNode a -> [LIR] -> IO [NodeId]
getSuccessors _ [] = error "Cannot examine empty program"
getSuccessors node (_:program:_) = do
  let blockid = fst wn
      blocks' = makeBlockMap $ blocks program
  unless (M.member blockid blocks') $
    error $ unwords ["Block id"
                    , show blockid
                    , "outside of range"
                    , show blocks'
                    ]
  let block  = blocks' M.! blockid
      nodes' = makeNodeMap $ nodes block
      nodeid = snd wn
  unless (M.member nodeid nodes') $
    error $ unwords ["Node id"
                    , show nodeid
                    , "outside of range"
                    , show nodes'
                    ]
  let node'   = nodes' M.! nodeid
      ns      = map id $ nodes block
      idx     = fromJust $ L.elemIndex nodeid ns
      nextIdx = idx + 1
  case successors node' of
       -- If there are no other block successors, the next node
       -- is either (1) nothing or (2) the next node in the block
       [] -> if nextIdx >= length ns
             -- No blocks afterwards if we're the last node in a
             -- block with no successors
             then return []
             -- Next node
             else return $ [(fst wn, ns !! nextIdx)]
       -- If there are successors in other blocks, they are
       -- the first nodes of those blocks
       bs -> do
         -- the next node is not always '1'
         -- sometimes it is nine or some bullshit
         let fin = map (\b -> let nextBlock = blocks' M.! b
                             in case nodes nextBlock of
                                  []   -> error "Empty next block"
                                  n:ns -> (b, id n)
                       ) bs
         return fin
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

-- Analysis-specific class information

-- | We have to be able to combine two peices of analysis information
-- in order to check the program using Kilall's algorithm
-- We also need a transfer function that "propagates information thru an expression"
class Checkable a where
    meet :: a -> a -> IO a
    transfer :: [LIR] -> WorkNode a -> IO (WorkNode a)

-- | http://www.ccs.neu.edu/home/types/resources/notes/kildall/kildall.pdf
-- Kildall's algorithm for computing program information to a fixed point
kildall :: (Checkable a, Eq a, Show a)
        => [WorkNode a] -- ^ Worklist
        -> Store a -- ^ Store of analysis information
        -> [LIR]
        -> IO (Store a)
kildall [] store _ = return store
kildall (elem:rest) store lir = do
  let incomingState = nodeState elem
      currentState = infoAt elem store
  newState <- meet incomingState currentState
  if newState == currentState
  then kildall rest store lir
  else do
    succs <- getSuccessors elem lir
    let newStore = updateStore elem newState store
    transferredState <- transfer lir $ WorkNode (workNode elem) newState
    next <- forM succs $ \e -> return $ WorkNode e (nodeState transferredState)
    kildall (rest++next) newStore lir

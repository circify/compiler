module Static.Kildall where
import           AST.LIR
import           AST.Regalloc
import qualified Data.Map     as M

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
data Store a = Store (M.Map NodeId a)

-- | Information of node relationships
type Successors = M.Map NodeId [NodeId]

-- Functions for interacting with successors

getSuccessors :: WorkNode a -> Successors -> [NodeId]
getSuccessors node succs = M.findWithDefault [] (workNode node) succs

-- Functions for interacting with the store

infoAt :: WorkNode a -> Store a -> a
infoAt node (Store store) = case M.lookup (workNode node) store of
                      Just item -> item
                      Nothing -> error $ unwords [ "Could not find store info for"
                                                 , show $ workNode node
                                                 ]

updateStore :: WorkNode a -> a -> Store a -> Store a
updateStore node item (Store store) = Store $ M.insert (workNode node) item store

-- Analysis-specific class information

-- | We have to be able to combine two peices of analysis information
-- in order to check the program using Kilall's algorithm
-- We also need a transfer function that "propagates information thru an expression"
class Checkable a where
    meet :: a -> a -> a
    transfer :: WorkNode a -> WorkNode a

-- | http://www.ccs.neu.edu/home/types/resources/notes/kildall/kildall.pdf
-- Kildall's algorithm for computing program information to a fixed point
kildall :: (Checkable a, Eq a)
        => [WorkNode a] -- ^ Worklist
        -> Store a -- ^ Store of analysis information
        -> Successors
        -> Store a
kildall [] store _ = store
kildall (elem:rest) store succs =
  let incomingState = nodeState elem
      currentState = infoAt elem store
      newState = incomingState `meet` currentState
  in if newState == currentState
     then kildall rest store succs
     else let newStore = updateStore elem newState store
              newElems = map (\e -> transfer $ WorkNode e newState) $ getSuccessors elem succs
          in kildall (rest++newElems) newStore succs

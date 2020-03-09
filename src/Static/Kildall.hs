module Static.Kildall where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (unless)
import           Data.List     (elemIndex)
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
data Store a = Store (M.Map NodeId a)
             deriving (Show)

-- Turn this into all program information
-- | Information of node relationships
type Successors = M.Map NodeId [NodeId]

-- Functions for interacting with successors

getSuccessors :: (Show a) => WorkNode a -> [LIR] -> IO [NodeId]
getSuccessors _ [] = error "Cannot examine empty program"
getSuccessors node (_:program:_) = do
  putStrLn $ unwords ["Getting succs for", show node]
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
  let node' = nodes' M.! nodeid
  case successors node' of
       -- If there are no other block successors, the next node
       -- is either (1) nothing or (2) the next node in the block
       [] -> if M.size nodes' == (fromIntegral $ snd wn)
             -- No blocks afterwards if we're the last node in a
             -- block with no successors
             then do
               print "returning without succs"
               print nodeid
               return []
             -- Next node
             else do
               let ns  = map id $ nodes block
                   idx = fromJust $ elemIndex nodeid ns
               print idx
               print ns
               print $ ns !! idx
               -- since nodes are 1-indexed, the next node is actually
               -- at the node index of the current node
               return [(fst wn, ns !! idx)]
       -- If there are successors in other blocks, they are
       -- the first nodes of those blocks
       bs -> do
         let rs = map (\b -> (b, 1)) bs
         print "Returning with succs"
         print rs
         return rs
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
    meet :: a -> a -> a
    transfer :: [LIR] -> WorkNode a -> WorkNode a

-- | http://www.ccs.neu.edu/home/types/resources/notes/kildall/kildall.pdf
-- Kildall's algorithm for computing program information to a fixed point
kildall :: (Checkable a, Eq a, Show a)
        => [WorkNode a] -- ^ Worklist
        -> Store a -- ^ Store of analysis information
        -> [LIR]
        -> IO (Store a)
kildall [] store _ = return store
kildall (elem:rest) store lir = do
  print "Starting kildall again"
  let incomingState = nodeState elem
      currentState = infoAt elem store
      newState = incomingState `meet` currentState
  print newState
  print currentState
  if newState == currentState
  then kildall rest store lir
  else do
    succs <- getSuccessors elem lir
    print "Got succs"
    let newStore = updateStore elem newState store
        newElems = map (\e ->
                            transfer lir $ WorkNode e newState
                       ) succs
    print "After new elems"
    kildall (rest++newElems) newStore lir

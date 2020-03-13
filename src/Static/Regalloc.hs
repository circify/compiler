{-# LANGUAGE OverloadedStrings #-}
module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad  (foldM, unless, when)
import           Data.List      (nub)
import qualified Data.Map       as M
import           Data.Maybe     (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Set       as S
import           Prelude        hiding (id)
import           Static.Kildall

{-|

Register allocation translation validation as presented in:
https://xavierleroy.org/publi/validation-regalloc.pdf

-}

data Loc = Reg RegisterName
         | SSlot StackSlot
         | ASlot ArgumentIndex
         deriving (Eq, Ord, Show)

data RegisterState = RegMap { regmap :: (M.Map Loc (S.Set VirtualRegister)) }
                   | Error String
                   | EmptyMap
                   | Start
                   deriving (Ord, Show)

instance Eq RegisterState where
    (RegMap m) == (RegMap n) = m == n
    Error{}    == Error{}    = True
    Error{}    == _          = False
    _          == Error{}    = False
    EmptyMap   == EmptyMap   = True
    EmptyMap   == _          = False
    _          == EmptyMap   = False
    Start      == _          = False
    _          == Start      = False

isStart :: RegisterState -> Bool
isStart Start = True
isStart _     = False

isError :: RegisterState -> Bool
isError Error{} = True
isError _       = False

isEmpty :: RegisterState -> Bool
isEmpty EmptyMap = True
isEmpty _        = False

getVirtFromAllocation :: LAllocation -> Maybe VirtualRegister
getVirtFromAllocation alloc =
  case alloc of
    LUseAllocation{} -> Just $ virtualRegister alloc
    _                -> Nothing

getLocFromAllocation :: LAllocation -> Maybe Loc
getLocFromAllocation alloc =
  case alloc of
    LGeneralRegAllocation{} -> Just $ Reg $ greg alloc
    LFloatRegAllocation{}   -> Just $ Reg $ freg alloc
    LStackSlotAllocation{}  -> Just $ SSlot $ slot alloc
    LArgumentAllocation{}   -> Just $ ASlot $ lindex alloc
    _                       -> Nothing

getVirtFromDefinition :: LDefinition -> VirtualRegister
getVirtFromDefinition = virtualReg

getLocFromDefinition :: LDefinition -> Maybe Loc
getLocFromDefinition def =
  case output def of
    Nothing    -> Nothing
    Just alloc -> getLocFromAllocation alloc

getLocsFromMove :: LMove -> (Loc, Loc)
getLocsFromMove move =
  let fromLoc = getLocFromAllocation $ from move
      toLoc   = getLocFromAllocation $ to move
  in if isNothing fromLoc || isNothing toLoc
  then error $ unwords ["Move from unknown location", show move]
  else (fromJust fromLoc, fromJust toLoc)

-- Get map information for a node

getDefInfo :: [LDefinition] -> [(VirtualRegister, Loc)]
getDefInfo def = catMaybes $ map getLocInfo def
  where getLocInfo d = case getLocFromDefinition d of
                         Nothing -> Nothing
                         Just rr -> Just (getVirtFromDefinition d, rr)

--
-- Helpers for the transfer function
--

getNodeMoveInfo :: LNode -> [(Loc, Loc)]
getNodeMoveInfo node =
  let op = operation node
  in case op of
    LMoveGroupOp{} -> map getLocsFromMove $ moves op
    _              -> error "Cannot get move info from non-move node"

lookupNode :: LIR
           -> WorkNode a
           -> IO LNode
lookupNode lir (WorkNode (bid, nid) _) = do
  let bs     = makeBlockMap $ blocks lir
  unless (M.member bid bs) $
    error $ unwords [show bid, "not in", show bs]
  let block  = bs M.! bid
      ns     = makeNodeMap $ nodes block
  unless (M.member nid ns) $
    error $ unwords [show nid, "not in", show ns]
  let ret = ns M.! nid
  return ret

---
--- The transfer function
---

initList :: [LIR] -> [WorkNode RegisterState]
initList [_,a] =
  concatMap (\b -> map (\n -> WorkNode (blockId b, id n) Start) $ nodes b) $ blocks a
initList _     = error "Unexpected number of IRs"

initState :: [LIR] -> Store RegisterState
initState lirs =
  let nodes = map workNode $ initList lirs
  in Store $ foldl (\m n -> M.insert n Start m) M.empty nodes

meet' :: RegisterState
      -> RegisterState
      -> [LIR]
      -> WorkNode a
      -> IO RegisterState
meet' r1 r2 [b,a] node
  | isError r1 = return r1
  | isError r2 = return r2
  | isEmpty r1 = return r2
  | isEmpty r2 = return r1
  | isStart r1 && isStart r2 = return EmptyMap
  | isStart r1 = return r2
  | isStart r2 = return r1
  | otherwise = return $ RegMap $ M.unionWith S.union (regmap r1) (regmap r2)

transfer' :: [LIR] -> WorkNode RegisterState -> IO (WorkNode RegisterState)
transfer' [b, a] node = do
  afterNode <- lookupNode a node
  let curState = nodeState node
  case operation afterNode of
    -- swap any RRs that must be swapped according to the move group
    LMoveGroupOp{} -> do
      let moves = getNodeMoveInfo afterNode
      return $ makeNode $ foldl (\m (loc1, loc2) -> switchInMap loc1 loc2 m) curState moves
    -- if its a phi, check the phi well-formed-ness condition
    -- ie rr = phi rr1, rr2, rr3 => rr = rr1 = rr2 = rr3....
    LOp "Phi" -> do
      let ds = getDefInfo $ defs afterNode
      case ds of
        [(vr, rr)] -> do
          let rrs = nub $ catMaybes $ map getLocFromAllocation $ operands afterNode
          return $ makeNode $ if [rr] == rrs
          then foldl (\m (v, k) -> resetInMap k v m) curState ds
          else Error "Unmoved phi argument"
        _ -> error "Malformed phi"
    -- not a special node
    _              -> do
      beforeNode <- lookupNode b node
      let operandsBefore = map getVirtFromAllocation $ operands beforeNode
          operandsAfter  = map getLocFromAllocation $ operands afterNode
      unless (length operandsBefore == length operandsAfter) $ error "Malformed IR"
          -- check for any conflicts with used operands
      newRegs <- foldM (\m (a, b) -> if isJust b && isJust a
                                     then do return $ addToMap (fromJust a) (fromJust b) m
                                     else return m
                       ) curState $ zip operandsAfter operandsBefore
          -- define new operands
      let ts = getDefInfo $ temps afterNode
          ds = getDefInfo $ defs afterNode
          newRegs' = foldl (\m (v, k) -> resetInMap k v m) newRegs $ ts ++ ds
      return $ makeNode newRegs'

        -- "Phi" -> return $ if map snd (getDefInfo (defs afterNode)) == nub (catMaybes operandsAfter)
        --          then makeNode newRegs'
        --          else makeNode $ Error $ unwords ["Badly formed phi"
        --                                          , show $ workNode node
        --                                          ]
    where makeNode ns = WorkNode (workNode node) ns
          resetInMap rr vr m = doToMap (\m -> M.insert rr (S.singleton vr) m) m
          addToMap rr vr m = case m of
                               RegMap m -> case M.lookup rr m of
                                 Nothing -> RegMap $ M.insert rr (S.singleton vr) m
                                 Just vs -> if vs == S.singleton vr
                                            then RegMap m
                                            else Error $ unwords ["Conflict at"
                                                                 , show rr
                                                                 ]
                               Error{} -> m
                               EmptyMap -> RegMap $ M.fromList [(rr, S.singleton vr)]
                               _ -> EmptyMap
          switchInMap from to m = doToMap (\m -> case M.lookup from m of
                                              Nothing -> m
                                              Just vs -> M.insert to vs $ M.delete from m
                                          ) m

doToMap :: (M.Map Loc (S.Set VirtualRegister) -> M.Map Loc (S.Set VirtualRegister))
        -> RegisterState
        -> RegisterState
doToMap fn m = case m of
                 Error{}  -> m
                 RegMap m -> RegMap $ fn m
                 EmptyMap -> RegMap $ fn M.empty
                 _        -> EmptyMap

instance Checkable RegisterState where
    meet = meet'
    transfer = transfer'


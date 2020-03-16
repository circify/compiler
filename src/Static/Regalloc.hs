{-# LANGUAGE OverloadedStrings #-}
module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad  (foldM, forM_, unless, when)
import           Data.List      (elem, intercalate, isInfixOf, nub)
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
         deriving (Eq, Ord)

instance Show Loc where
    show (Reg r)   = show r
    show (SSlot n) = 's':(show n)
    show (ASlot a) = 'a':(show a)

data RegisterState = RegMap { regmap :: (M.Map Loc (S.Set VirtualRegister)) }
                   | Error String
                   | Start

instance Show RegisterState where
    show (RegMap m) =
      M.foldlWithKey (\str k v -> str ++ "\n" ++ show k ++ " : " ++ show v) "" m
    show Start = "Start"
    show (Error str) = "ERROR:" ++ show str

instance Eq RegisterState where
    (RegMap m) == (RegMap n) = m == n
    Error{}    == Error{}    = True
    Error{}    == _          = False
    _          == Error{}    = False
    Start      == _          = False
    _          == Start      = False

isStart :: RegisterState -> Bool
isStart Start = True
isStart _     = False

isError :: RegisterState -> Bool
isError Error{} = True
isError _       = False

isMap :: RegisterState -> Bool
isMap RegMap{} = True
isMap _        = False

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
      -> IO RegisterState
meet' r1 r2
  | isError r1 = return r1
  | isError r2 = return r2
  | isStart r1 && isStart r2 = return $ RegMap M.empty
  | isStart r1 = return r2
  | isStart r2 = return r1
  | otherwise = return $ RegMap $ M.unionWith S.union (regmap r1) (regmap r2)

transfer' :: [LIR] -> WorkNode RegisterState -> IO (WorkNode RegisterState)
transfer' [b, a] node = do
  afterNode <- lookupNode a node
  let curState = nodeState node
--  sanityCheckResult node afterNode curStatecurState

  regs <- case operation afterNode of
    LMoveGroupOp{} -> do
      r <- transferMove curState afterNode
      return r
    LOp "Phi"      -> do
      beforeNode <- lookupNode b node
      state <- transferPhi curState beforeNode afterNode
      return state
    _              -> do
      beforeNode <- lookupNode b node
      r <- transferOther curState beforeNode afterNode
      return r

  -- when (workNode node == (13, 224)) $ do
  --   putStrLn "CULPRIT: -----------------------------------"
  --   print afterNode
  --   putStrLn "Before:"
  --   print curState
  --   putStrLn "After:"
  --   print regs
  return $ makeNode regs
    where makeNode ns = WorkNode (workNode node) ns

resetInMap rr vr m = doToMap (\m -> M.insert rr (S.singleton vr) m) m

---
--- Logic for the transfer functions
---

-- | Swap the real registers that must be swapped according to the move group.
-- If there is not yet information flowing in about one side of the swap, delete
-- both sides from the map. This way the register state won't cause errors when it
-- flows to later nodes.
transferMove :: RegisterState -> LNode -> IO RegisterState
transferMove curState after = do
  let moves   = getNodeMoveInfo after
      toMerge = foldl (\m (from, to) -> putInMap from to m) M.empty moves
      merged  = foldl (\m (from, to) -> case M.lookup to toMerge of
                                            Nothing -> doToMap (M.delete to) m
                                            Just vs -> doToMap (M.insert to vs) m
                      ) curState moves
  return merged
  where putInMap from to map = case curState of
                                 RegMap m -> case M.lookup from m of
                                               Nothing -> map
                                               Just vs -> M.insert to vs map
                                 _        -> map

-- | If its a phi, check the phi well-formed-ness condition
-- After: rr = phi vr1...vrn
-- Lookup up vr1..vrn in the
transferPhi :: RegisterState -> LNode -> LNode -> IO RegisterState
transferPhi curState before after = do
  let ops' = catMaybes $ map getVirtFromAllocation $ operands before
      defs' = getDefInfo $ defs after
  case defs' of
    [(vr, rr)] -> do
               let usedRrs = map (\vr -> case curState of
                                           RegMap m -> M.keys $ getRrsFromVr vr m
                                           _ -> []
                                 ) ops'
               if null usedRrs || all (\rs -> if null rs then True else rr `elem` rs) usedRrs
               then return $ resetInMap rr vr curState
               else return $ Error $ unwords [ "Broken phi"
                                             , show before
                                             , show after
                                             , show curState
                                             , show usedRrs
                                             , show rr
                                             ]
  where getRrsFromVr vr m = M.filter (S.member vr) m

-- | Check all uses against the current state of the register map
-- Then add all definitions to the register map
transferOther :: RegisterState -> LNode -> LNode -> IO RegisterState
transferOther curState before after = do
  let operandsBefore = map getVirtFromAllocation $ operands before
      operandsAfter  = map getLocFromAllocation $ operands after
  unless (length operandsBefore == length operandsAfter) $ error "Malformed IR"
  -- check for any conflicts with used operands
  newRegs <- foldM (\m (a, b) -> if isJust b && isJust a
                                 then return $ checkInMap (fromJust a) (fromJust b) m
                                 else return m
                   ) curState $ zip operandsAfter operandsBefore
  -- define new operands
  let ts = getDefInfo $ temps after
      ds = getDefInfo $ defs after
  return $ foldl (\m (v, k) -> resetInMap k v m) newRegs $ ts ++ ds
  -- if there's a conflict, return an error. otherwise return the map
  where checkInMap k v m = case m of
          RegMap m' -> case M.lookup k m' of
                         Nothing -> m
                         Just v' | v' == (S.singleton v) -> m
                         Just vs -> Error $ unwords [ "Conflict at"
                                                    , show k
                                                    , show vs
                                                    ]
          Start     -> RegMap M.empty
          _         -> m



doToMap :: (M.Map Loc (S.Set VirtualRegister) -> M.Map Loc (S.Set VirtualRegister))
        -> RegisterState
        -> RegisterState
doToMap fn m = case m of
                 Error{}  -> m
                 RegMap m -> RegMap $ fn m
                 _        -> RegMap M.empty

instance Checkable RegisterState where
    meet = meet'
    transfer = transfer'


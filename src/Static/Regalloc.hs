module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad  (foldM, unless, when)
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

data RegisterState = RegMap { regmap :: (M.Map Loc VirtualRegister) }
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

getNodeInfo :: LNode -> LNode -> RegisterState
getNodeInfo nodeBefore nodeAfter =
  let operandsBefore = map getVirtFromAllocation $ operands nodeBefore
      operandsAfter  = map getLocFromAllocation $ operands nodeAfter
      operandMap     = M.fromList $
                       catMaybes $ map (\(b, a) -> if isJust b && isJust a
                                       then Just (fromJust a, fromJust b)
                                       else Nothing
                                       ) $ zip operandsBefore operandsAfter
      operandState   = if null operandMap then EmptyMap else RegMap operandMap
      ts             = getDefInfo $ temps nodeAfter
      ds             = getDefInfo $ defs nodeAfter
  in foldl (\m (v,k) ->
             case m of
               EmptyMap  -> RegMap $ M.fromList [(k, v)]
               Error{}   -> m
               Start     -> RegMap $ M.fromList [(k, v)]
               RegMap m' -> RegMap $ M.insert k v m'
           ) operandState $ ts ++ ds

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
  | otherwise = do
      let r1ks    = S.fromList $ M.keys r1'
          r2ks    = S.fromList $ M.keys r2'
          allKeys = S.union r1ks r2ks
      node' <- lookupNode a node
      r' <- foldM (\m' k -> do
        if S.member k r1ks && S.member k r2ks
        -- We have a conflict.
        -- Is it an ok conflict (introduced by a new def?)
        -- Or a bad conflict (use => different allocation for some register)
        then do
          let v1 = r1' M.! k
              v2 = r2' M.! k
              ds = getDefInfo (defs node') ++ getDefInfo (temps node')
          case ds of
            -- The conflict is just the same value. Fine!
            _  | v1 == v2 -> do
               return $ addToMap k v1 m'
            -- There are no definitions that could excuse the conflict
            [] -> do
               return $ Error $ unwords [ "Conflict at register"
                                        , show k
                                        ]
            -- There is a definition. Does it excuse the conflict
            defs -> do
              let ds = filter (\(vr, rr) -> rr == k) defs
              print defs
              print ds
              print node'
              return $ case ds of
                [(vr, rr)] -> addToMap rr vr m'
                _ -> Error $ unwords [ "Conflict at register"
                                      , show k
                                      ]
          else return $ if S.member k r1ks
               then addToMap k (r1' M.! k) m'
               else addToMap k (r2' M.! k) m'
                  ) EmptyMap allKeys
      return $ r'
      where r1' = regmap r1
            r2' = regmap r2
            addToMap k v m = case m of
                               Start     -> RegMap $ M.fromList [(k,v)]
                               Error{}   -> m
                               EmptyMap  -> RegMap $ M.fromList [(k,v)]
                               RegMap m' -> RegMap $ M.insert k v m'

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

transfer' :: [LIR] -> WorkNode RegisterState -> IO (WorkNode RegisterState)
transfer' [b, a] node = do
  afterNode <- lookupNode a node
  case operation afterNode of
    LMoveGroupOp{} -> do
        let curState = nodeState node
            newMap = foldl (\m (from, to) ->
                      case m of
                        Start    -> EmptyMap
                        EmptyMap -> EmptyMap
                        Error{}  -> m
                        RegMap m -> case M.lookup from m of
                                      Nothing -> RegMap m
                                      Just v -> let newM = M.delete from m
                                                in RegMap $ M.insert to v m
                           ) curState $ getNodeMoveInfo afterNode
        return $ WorkNode (workNode node) newMap
    _ -> do
      beforeNode <- lookupNode b node
      newMap <- meet (nodeState node) (getNodeInfo beforeNode afterNode) [b,a] node
      return $ WorkNode (workNode node) newMap
transfer' _ _         = error "Unexpected number of LIRs"

instance Checkable RegisterState where
    meet = meet'
    transfer = transfer'


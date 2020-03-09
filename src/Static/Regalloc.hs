module Static.Regalloc where
import           AST.LIR
import           AST.Regalloc
import qualified Data.Map       as M
import           Data.Maybe     (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Set       as S
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
                   deriving (Eq, Ord, Show)

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

getDefInfo :: [LDefinition] -> Maybe (VirtualRegister, Loc)
getDefInfo def = case def of
  []    -> Nothing
  [def] -> case getLocFromDefinition def of
             Nothing -> Nothing
             Just rr -> Just (getVirtFromDefinition def, rr)
  _     -> error "Unexpected number of definitions in node"

--
-- Helpers for the transfer function
--

getNodeMoveInfo :: LNode -> [(Loc, Loc)]
getNodeMoveInfo node =
  let op = operation node
  in case op of
    LMoveGroupOp{} -> map getLocsFromMove $ moves op
    _              -> error "Cannot get move info from non-move node"

getNodeDefInfo :: LNode -> RegisterState
getNodeDefInfo node =
  case getDefInfo $ defs node of
    Nothing       -> EmptyMap
    Just (vr, rr) -> RegMap $ M.fromList [(rr, vr)]

getNodeUseInfo :: LNode -> LNode -> RegisterState
getNodeUseInfo nodeBefore nodeAfter =
  let operandsBefore = map getVirtFromAllocation $ operands nodeBefore
      operandsAfter  = map getLocFromAllocation $ operands nodeAfter
      operandMap     = catMaybes $ map (\(b, a) -> if isJust b && isJust a
                                       then Just (fromJust a, fromJust b)
                                       else Nothing
                                       ) $ zip operandsBefore operandsAfter
      tempsBefore    = map getVirtFromDefinition $ temps nodeBefore
      tempsAfter     = map getLocFromDefinition $ temps nodeAfter
      tempsMap       = catMaybes $ map (\(b, a) -> if isJust a
                                       then Just (fromJust a, b)
                                       else Nothing
                                       ) $ zip tempsBefore tempsAfter
  in foldl (\m (k, v) ->
             case m of
               EmptyMap -> RegMap $ M.fromList [(k, v)]
               Error{}  -> m
               RegMap m ->
                 if M.member k m
                 then case m M.! k of
                        v' | v == v' -> RegMap m
                        e  -> Error $ unwords $ [show k, "cannot be", show v, "and", show e]
                 else RegMap $ M.insert k v m
           ) EmptyMap $ tempsMap ++ operandMap

---
--- The transfer function
---

meet :: RegisterState
     -> RegisterState
     -> RegisterState
meet r1 r2
  | isError r1 = r1
  | isError r2 = r2
  | isEmpty r1 = r2
  | isEmpty r2 = r1
  | otherwise = let keys = S.union (S.fromList $ M.keys r1') (S.fromList $ M.keys r2')
                    r'    = foldl (\m' k ->
                      if (r1' M.! k) == (r2' M.! k)
                      then m'
                      else Error $ unwords $ ["Conflicting virtual regs for phys reg"
                                             , show k
                                             , ":"
                                             , show $ r1' M.! k
                                             , ","
                                             , show $ r2' M.! k
                                             ]
                               ) EmptyMap keys
                in if isError r'
                   then r'
                   else RegMap $ M.union r1' r2'
      where r1' = regmap r1
            r2' = regmap r2

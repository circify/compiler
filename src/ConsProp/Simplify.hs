module ConsProp.Simplify where

import Language.C.Syntax.AST
import Language.C.System.GCC
import ConsProp.Apron.AbstractMonad
import ConsProp.Apron.Abstract1
import ConsProp.Init

isBotState :: AbsState -> Abstract Bool
isBotState (State a _) = do
  abs <- a
  abstractIsBottom abs

-- Stage 1: The only way a Bottom might occur is after a condition
--          Since we converted all loops into if-else statements, we only need
--          to deal with if-else statements.
--          Moreoever, if the "if" part is bottom and else-part does not exist
--          then we can skip the if-statement directly.
-- Stage 2: Eliminate functions that have never been called
--          STILL UNDER IMPLEMENTATION
removeBot :: Abstract (CTranslationUnit AbsState) -> Abstract (CTranslationUnit AbsState)
removeBot c = do
  CTranslUnit extDecls st <- c
  nExtDecls <- rbEDLst extDecls
  return (CTranslUnit nExtDecls st)

-- If an ExternalDeclaration is Bottom, it could be
-- 1. The function is never called
-- 2. There are infinite loops / recursions within the function
-- Thus we cannot really make any conclusion
rbEDLst :: [CExternalDeclaration AbsState] -> Abstract [CExternalDeclaration AbsState]
rbEDLst []       = return []
rbEDLst (ed:eds) = do
  newEd  <- rbExtDecl ed
  newEDs <- rbEDLst eds
  return ([newEd] ++ newEDs)

rbExtDecl :: CExternalDeclaration AbsState -> Abstract (CExternalDeclaration AbsState)
rbExtDecl ed@(CDeclExt decl) = return ed
rbExtDecl (CFDefExt func)    = do
  nFunc <- rbFunc func
  return (CFDefExt nFunc)
rbExtDecl _ = error "CAsmExt not implemented"

rbFunc :: CFunctionDef AbsState -> Abstract (CFunctionDef AbsState)
rbFunc (CFunDef a b c stmt st) = do
  (_, nStmt) <- rbStmt stmt
  return (CFunDef a b c nStmt st)

-- Use the Bool to record if the statement will not be evaluated
-- In particular, it will be True if the statements in if will not be executed
rbStmt :: CStatement AbsState -> Abstract (Bool, CStatement AbsState)
rbStmt (CCompound ids cbis st) = do
  bot   <- isBotState st
  -- If rbStmt will not be evaluated, then the CBIs in it do not matter
  nCbis <- case bot of
             True  -> return cbis
             False -> rbCBIs cbis
  return (bot, CCompound ids nCbis st)
rbStmt s@(CExpr _ st) = do
  bot <- isBotState st
  return (bot, s)
rbStmt s@(CReturn _ st) = do
  bot <- isBotState st
  return (bot, s)
rbStmt s@(CIf cons tstmt Nothing st) = do
  -- If tstmt will not be evaluated, then the entire if statement is pointless
  (tBot, ntstmt) <- rbStmt tstmt
  return (tBot, CIf cons ntstmt Nothing st)
rbStmt s@(CIf cons tstmt (Just fstmt) st) = do
  (tBot, tstmt1) <- rbStmt tstmt
  (fBot, nfstmt) <- rbStmt fstmt
  -- For the if-else statment:
  -- if T then A else B
  -- if A is not evaluated, we can change the statement to
  -- if !T then B
  let negCons = CUnary CNegOp cons st
  case (tBot, fBot) of
    (False, False) -> return (False, s)
    -- No else
    (False, True)  -> return (False, CIf cons tstmt Nothing st)
    -- No if
    (True, False)  -> return (False, CIf negCons fstmt Nothing st)
    -- No if and no else
    (True, True)   -> return (True, s)
rbStmt s = error ("Statement cannot be simplified: " ++ (show s))

rbCBIs :: [CCompoundBlockItem AbsState] -> Abstract [CCompoundBlockItem AbsState]
rbCBIs []         = return []
rbCBIs (cbi:cbis) = do
  (bot, nCbi) <- rbCBI cbi
  nCbis <- rbCBIs cbis
  case bot of
    False -> return ([nCbi] ++ nCbis)
    True  -> return nCbis

rbCBI :: CCompoundBlockItem AbsState -> Abstract (Bool, CCompoundBlockItem AbsState)
rbCBI (CBlockStmt stmt) = do
  (bot, nStmt) <- rbStmt stmt
  return (bot, CBlockStmt nStmt)
rbCBI cbi@(CBlockDecl decl) = return (False, cbi)
rbCBI _ = error "CBI nested function type not implemented"

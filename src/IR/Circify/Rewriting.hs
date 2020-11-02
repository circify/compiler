{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module IR.Circify.Rewriting
  ( inlineFunctions,
    localValueNumbering
  )
where
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State.Strict
import           Util.Cfg
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as M
import           Numeric.Natural
import           IR.Circify.Control
import           Codegen.Circom.C                    -- CState getFunction

-- helper
replace (x,y) param_list = map (\x' -> if x'==x then y else x') param_list
replace (x,y) param = if (param == x) then y else param

-- TODO make return id
repl_list (h:t) params
  | length (h:t) == 0 = params
  | length (h:t) == 1 = replace h params
  | otherwise = repl_list t (replace h params)

paramsDeclList (CDeclr decl) = declr --further reduction?
paramsFunDef (CFunDef _ (CDecl specifiers declist _ ) _ _ _ ) = [paramsDecList d | d <- declist]
bodyFunDef (CFunDef _ _ _ cstmt _ ) = cstmt

--replaceArgs :: [term] -> type of params?? -> Control term
replaceArgs assgnMap code = case code of
  (For iter star end body)    -> For iter star end (replaceArgs assgnMap body)
  (While cond body)           -> While cond (replaceArgs assgnMap body)
  (If cond thenBody elseBody) -> If cond (replaceArgs assgnMap thenBody) (replaceArgs assgnMap elseBody)
  (Seq left right)            -> Seq (replaceArgs assgnMap left) (replaceArgs assgnMap right)
  (Empty)                     -> Empty
  (Call func params)          -> Call func (repl_list assgnMap params) -- rewrite the list of parameters if any match on inner function call
  (CStat cs _) -> case cs of
        (CExpr ce _) -> case ce of -- find assgnMap names and replace
              (CVar id a)                   -> CVar (repl_list assgnMap id) a --TODO machinery
              (CAssign op lhs rhs a )       -> CAssign op (replaceArgs assgnMap lhs) (replaceArgs assgnMap rhs) a
              (CBinary op left right a )    -> CBinary op (replaceArgs assgnMap left) (replaceArgs assgnMap right) a
              (CUnary op arg a )            -> CUnary op (replaceArgs assgnMap arg) a
              (CIndex array index a)        -> CIndex (replaceArgs assgnMap array) (replaceArgs assgnMap index) a

              (CMember struct id isArrow a) -> CMember (replaceArgs assgnMap struct) (repl_list assgnMap id) isArrow a

              (CCast decl expr a)           -> CCast decl (replaceArgs assgnMap expr) a
              -- don't need to mess with new declarations
              (CCond cond tBr fBr a)        -> CCond (replaceArgs assgnMap cond) (replaceArgs assgnMap tBr) (replaceArgs assgnMap fBr) a
              (CSizeofExpr e a)             -> ?
              (CSizeofType decl a)          -> ?

              other                         -> other -- CCall, CConst, etc. do not need to be handled
          other                     -> other
          --  I don't think CStat that are not CExpr need to be handled (control flow, CCompound, CBlockDecl, labels, etc....)
          --  as they SHOULD be handled when converting to IR layer



inlineFunctions :: Control t => Control t
inlineFunction bode = case code of
  (For iter star end body)      -> For iter star end (inlineFunctions body)
  (While cond body)             -> While cond (inlineFunctions body)
  (If cond thenBody elseBody)   -> If cond (inlineFunctions thenBody) (inlineFunctions elseBody)
  (Seq left right)              -> Seq (inlineFunctions left) (inlineFunctions right)
  (Empty)                       -> Empty
  (Call funcName args)          -> inlineFunctions argRpBody
  other                         -> other -- we assume all function calls are tagged with Call
    where funcDecl  = getFunction funcName
          params    = paramsFunDef funcDecl
          body      = bodyFunDef funcDecl
          assgn     = if (length params == length args) then (zip params args) else (error "arguments don't match")
          argRpBody = replaceArgs assgn body



localValueNumbering :: Control t => Control t
localValueNumbering block

--break down control flow into vars
-- check for algebraic identities
--store vars in map
-- if op commutative, sort


data lvnValue = {op, one, two}

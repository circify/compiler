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
replace (x,y) params = map (\x' -> if x'==x then y else x') params

repl_list (h:t) params
  | length (h:t) == 0 = params
  | length (h:t) == 1 = replace h params
  | otherwise = repl_list t (replace h params)

--pickApartFunDef (CFunDef _ declarator _  cstmt _ ) = getSubStmts cstmt


--replaceArgs :: [term] -> type of params?? -> Control term
replaceArgs assgnMap code = case code of
  (For iter star end body) = For iter star end (replaceArgs assgnMap body)
  (While cond body) = While cond (replaceArgs assgnMap body)
  (If cond thenBody elseBody) = If cond (replaceArgs assgnMap thenBody) (replaceArgs assgnMap elseBody)
  (Seq left right) = Seq (replaceArgs assgnMap left) (replaceArgs assgnMap right)
  (Empty) = Empty
  (Call func params) = Call func (repl_list assgnMap params) -- rewrite the list of parameters if any match on inner function call
  (CStat cs) = (getSubStmts cs)




      --where [] <- getLabels cs
    -- find assgnMap names and replace




inlineFunctions :: Control t => Control t
inlineFunction bode = case code of
  (For iter star end body) = For iter star end (inlineFunctions body)
  (While cond body) = While cond (inlineFunctions body)
  (If cond thenBody elseBody) = If cond (inlineFunctions thenBody) (inlineFunctions elseBody)
  (Seq left right) = Seq (inlineFunctions left) (inlineFunctions right)
  (Empty) = Empty
  (Call funcName args) = inlineFunctions argRpBody
    where funcDecl = getFunction funcName
          params = -- get
          body = -- get
          assgn = zip params args
          argRpBody = replaceArgs assgn body



          unless (length params == length args)
            $  error
            $  "arguments don't match"

-- | Call { func :: String, args :: [term] }

-- replace args, maintin "control flow layer": assume params are also cterms
----- get parameters, map them
-- recurse





localValueNumbering :: Control t => Control t
localValueNumbering block

--break down control flow into vars
-- check for algebraic identities
--store vars in map
-- if op commutative, sort


data lvnValue = {op, one, two}

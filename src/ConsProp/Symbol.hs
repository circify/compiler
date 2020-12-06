{-# LANGUAGE ScopedTypeVariables #-}
module ConsProp.Symbol
  ( getSymT
  )
where

import           Control.Monad
import           Language.C
import           Language.C.Data.Ident
import           Language.C.Analysis
import           Language.C.System.GCC
import qualified Data.Maybe                    as Maybe
import qualified System.Environment            as Env
import           Text.PrettyPrint.HughesPJ

-- Given a filename, parse the C in that file
parseCPath :: String -> IO CTranslUnit
parseCPath file = do
  r <- parseCFile (newGCC "gcc") Nothing [] file
  return $ either (\e -> error $ "parse error: " ++ show e) id r

type GetTab a = Trav [(Doc, Position)] a

-- Convert raw declaration data to actual symbol table
genSymT :: [String] -> String -> [String]
genSymT [] prefix = []
genSymT (raw:raws) prefix =
  case (last raw) of
    -- use func@var to declare local variable
    '{' -> genSymT raws ((init raw) ++ "@")
    -- use function name to indicate the return value of that function
    -- since identifier names cannot repeat
    '}' -> [init raw] ++ genSymT raws ""
    -- use original name for global variables
    _   -> [prefix ++ raw] ++ (genSymT raws prefix)

-- DO NOT USE THIS DIRECTLY
-- Refer to getFullSymT in Init.hs instead
-- Traverse through the syntax tree and generate symbol table
getSymT :: String -> IO [String]
getSymT path = do
  tu <- parseCPath path
  let handleDecl decl = case decl of
        -- Functions
        DeclEvent (Declaration (Decl (VarDecl (VarName (Ident i _ _) _) _ _) _)) -> modifyUserState (++ [(i ++ "{")])
        DeclEvent (FunctionDef (FunDef (VarDecl (VarName (Ident i _ _) _) _ _) _ _)) -> modifyUserState (++ [(i ++ "}")])
        -- Arguments
        ParamEvent (ParamDecl (VarDecl (VarName (Ident i _ _) _) _ _) _) -> modifyUserState (++ [i])
        -- Global Variable
        DeclEvent (ObjectDef (ObjDef (VarDecl(VarName (Ident i _ _) _) _ _) _ _)) -> modifyUserState (++ [i])
        -- Local Variable
        LocalEvent (ObjectDef (ObjDef (VarDecl (VarName (Ident i _ _) _) _ _) _ _)) -> modifyUserState (++ [i])
        _           -> return ()
  let trav = withExtDeclHandler (analyseAST tu >> getUserState) handleDecl
  let r = runTrav [] trav
  let st :: [String] = either (error . show) fst r
  let st1 = genSymT st ""
  return st1

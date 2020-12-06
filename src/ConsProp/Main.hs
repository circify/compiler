module ConsProp.Main where

import           ConsProp.Init
import           ConsProp.Eval
import           ConsProp.Simplify
import           Language.C.Syntax.AST
import           ConsProp.Apron.AbstractMonad

constPropParse :: String -> IO CTranslUnit
constPropParse fileName = do
  iast <- analyzeAST fileName
  let nast = evalProg iast
  let fast = removeBot nast
  let ast  = reverseTo fast
  cpParseHelper ast

cpParseHelper :: Abstract CTranslUnit -> IO CTranslUnit
cpParseHelper atu = evalAbstract defaultState $ atu

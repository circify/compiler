module Analyze.Example where
import           Language.C.Data.Ident
import           Language.C.Data.Node
import           Language.C.Syntax.AST
import           Parser.C

-- | A simple user-defined AST annotation type
-- You can extend this if it would be helpful
-- For now it perserves source code location info
data Decorated a = Decorated { analysisInfo :: a
                             , locInfo      :: NodeInfo
                             }
                 deriving (Eq, Ord, Show)


decorate :: a -> NodeInfo -> Decorated a
decorate = Decorated

-- | Dummy state
data ExampleState = StartState | EndState
                  deriving (Eq, Ord, Show)

-- | Uses language-c's built-in (nice) annotation support
-- See here for more info:
-- https://hackage.haskell.org/package/language-c-0.8.3/docs/Language-C-Syntax-AST.html#g:9
initTo :: a -> CTranslUnit -> CTranslationUnit (Decorated a)
initTo startState t = decorate startState <$> t

-- | Initialize everything to a start state and then print the AST
analyzeAST :: String -> IO ()
analyzeAST name = do
  tu <- parseC name
  print $ initTo StartState tu

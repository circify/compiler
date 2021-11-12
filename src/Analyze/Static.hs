module Analyze.Static where
import           Control.Monad.Reader
import           Language.C
import           Language.C.Analysis
import           Util.Cfg             (Cfg)

-- We are going to do this in the stupidest possible way becase it's quicker
-- and I don't think language-c has exactly what we want.
-- We will have a nice, general framework in the Rust version

-- class Check a where
--     combine :: [a] -> a -> a
--     cTranslUnit :: CTranslUnit -> a -> IO a
--     cFunctionDef :: CFunctionDef b -> a -> IO a

slice :: CTranslUnit -> IO CTranslUnit
slice ast = do
  print "Checking:"
  check ast
  return ast

check :: (Show b) => CTranslationUnit b -> IO ()
check (CTranslUnit decls _) = forM_ decls $ checkDecls

checkDecls :: (Show b) => CExternalDeclaration b -> IO ()
checkDecls decls = case decls of
                     CDeclExt{}   -> return ()
                     CFDefExt fun -> checkFunctionDef fun
                     CAsmExt{}    -> return ()

checkFunctionDef :: (Show b) => CFunctionDef b -> IO ()
checkFunctionDef (CFunDef specs dec decs stmt _) = do
  checkDeclarator dec
  checkStmt stmt

checkDeclarator :: (Show b) => CDeclarator b -> IO ()
checkDeclarator (CDeclr (Just fName) _ _ _ _) = do
  print fName

checkStmt :: (Show b) => CStatement b -> IO ()
checkStmt stmt = case stmt of
                   CCompound _ items _ -> mapM_ checkCompoundBlockItem items
                   CExpr (Just expr) _ -> checkExpr expr
                   _                   -> print stmt

checkExpr :: (Show b) => CExpression b -> IO ()
checkExpr expr = case expr of
                   CBinary op e1 e2 _ -> print op
                   _                  -> return ()

checkCompoundBlockItem :: (Show b) => CCompoundBlockItem b -> IO ()
checkCompoundBlockItem item = case item of
                                CBlockStmt stmt -> checkStmt stmt
                                _               -> return ()



-- data Checker = Checker deriving (Eq, Ord, Show)

-- instance Check Checker where
--     combine cs c = c
--     cTranslUnit _ c = return c
--     cFunctionDef n@(CFunDef _ dec _ _ _) c = do
--                          print n
--                          return c


-- checkTranslUnit :: (Check a) => a -> CTranslUnit -> IO a
-- checkTranslUnit checker n@(CTranslUnit decls _) = do
--     result <- cTranslUnit n checker
--     results <- forM decls $ checkFunctionDef checker
--     -- check the decls
--     return result

-- checkFunctionDef :: (Check a, Show b) => a -> CExternalDeclaration b -> IO a
-- checkFunctionDef checker (CFDefExt fundef) = do
--   result <- cFunctionDef fundef checker
--   return result
-- checkFunctionDef checker _ = return checker





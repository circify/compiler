module ConsProp.Array where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Syntax.Constants
import Data.List (find)

-- A spinoff of the findScope function that avoids the problem of abstract monad
findVarName :: String -> String -> [String] -> String
findVarName varName funcName nameLst
  | funcName == "" = varName
  | otherwise =
    case (find (==localName) nameLst) of
      Nothing -> varName
      _       -> localName
    where localName = funcName ++ "@" ++ varName

-- Convert "a" [3, 4, 5] to ["a#0#0#0", "a#0#0#1", ...]
convertArray :: String -> String -> [Integer] -> [String] -> [String]
convertArray v f ns s = s ++ (convertArrayHelper var ns)
  where var    = findVarName v f s
        -- newStr = delete var s

convertArrayHelper :: String -> [Integer] -> [String]
convertArrayHelper var ns =
  foldl dimConcat [var] (map dimGen ns)

dimGen :: Integer -> [String]
dimGen 1 = ["#0"]
dimGen n
  | n > 1     = (dimGen (n - 1)) ++ ["#" ++ (show (n - 1))]
  | otherwise = error "Invalid array size!"

dimConcat :: [String] -> [String] -> [String]
dimConcat a b = foldl (++) [] (map (\p -> (map (\n -> p ++ n) b)) a)

-- Given a Symbol Table of a program, extract all static allocated arrays
-- to individual variables.
-- Naming convention: func@a -> func@a#0, func@a#1, ...
-- Assumption: Variable is never reassigned a different type
--
-- We want to traverse all declarations to find arrays
obtainArraySymT :: [String] -> CTranslationUnit a -> [String]
obtainArraySymT s (CTranslUnit extDecls _) =
  foldl oaExtDecl s extDecls

oaExtDecl :: [String] -> CExternalDeclaration a -> [String]
oaExtDecl s (CDeclExt decl) = oaDecl s "" decl
oaExtDecl s (CFDefExt func) = oaFunc s func
oaExtDecl _ _ = error "CAsmExt not implemented"

oaFunc :: [String] -> CFunctionDef a -> [String]
oaFunc s (CFunDef _ (CDeclr (Just (Ident f _ _)) (funDeclr:_) _ _ _) decls stmt _) =
  oaStmt fs f stmt
  where ns = case funDeclr of
               CFunDeclr (Right (funDecls, False)) _ _ -> foldDecl s funDecls
               _ -> s
        fs = foldDecl ns decls
        foldDecl m n = foldl (\a b -> oaDecl a f b) m n

oaFunc _ _ = error "Unsupported Function"

oaDecl :: [String] -> String -> CDeclaration a -> [String]
oaDecl s f (CDecl _ decls _) =
  foldl (\a b -> oaDeclHelper a f b) s decls
oaDecl _ _ _ = error "Invalid Declaration"

oaDeclHelper :: [String] -> String -> (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a)) -> [String]
oaDeclHelper s f (decl, _, _) =
  case decl of
    -- Array name: v, length: n
    Just (CDeclr (Just (Ident v _ _)) cds _ _ _) -> convertArray v f (getArrayLength cds) s
    _ -> s

-- Deal with the case of multiple-dimension arrays
getArrayLength :: [CDerivedDeclarator a] -> [Integer]
getArrayLength [] = []
getArrayLength (cd:cds) =
  case cd of
    CArrDeclr _ (CArrSize _ (CConst (CIntConst (CInteger n _ _) _))) _ -> [n] ++ is
    _ -> error "Unsupported Derived Declarator"
  where is = getArrayLength cds

oaStmt :: [String] -> String -> CStatement a -> [String]
oaStmt s _ (CExpr _ _ )  = s
oaStmt s _ (CReturn _ _) = s
oaStmt s f (CIf _ tstmt fstmt _)
  | ts == fs  = ts
  | otherwise = error "Please do not use array declarataion in if/else statements"
  where ts = oaStmt s f tstmt
        fs = case fstmt of
               Just a  -> oaStmt s f a
               Nothing -> s
oaStmt s f (CWhile _ stmt _ _)  = oaStmt s f stmt
oaStmt s f (CFor _ _ _ stmt _)  = oaStmt s f stmt
oaStmt s f (CCompound _ cbis _) = foldl (\a b -> oaCBI a f b) s cbis
oaStmt _ _ _ = error "Statement type not implemented"

oaCBI :: [String] -> String -> CCompoundBlockItem a -> [String]
oaCBI s f (CBlockStmt stmt) = oaStmt s f stmt
oaCBI s f (CBlockDecl decl) = oaDecl s f decl
oaCBI _ _ _ = error "CBI nested function type not implemented"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Codegen.C.AstUtil
  ( ctype
  , cDeclToType
  , baseTypeFromSpecs
  , cSplitDeclaration
  , nodeText
  , fnRetTy
  , fnInfo
  , nameFromFunc
  , derivedFromDeclr
  , identToVarName
  , identFromDeclr
  , isTypedef
  , storageFromSpec
  , isStorageSpec
  , defStruct
  , getStruct
  )
where
import           Codegen.C.Type
import           Codegen.Circify
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import           Language.C.Data.Ident
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants

ctype
  :: [CDeclSpec] -> [CDerivedDeclr] -> Circify Type term (Either String Type)
ctype tys ptrs = do
  ty <- baseTypeFromSpecs tys
  return $ ty >>= flip applyDerivations ptrs

fnRetTy :: CFunDef -> Circify Type term (Either String Type)
fnRetTy f = ctype (baseTypeFromFunc f) (ptrsFromFunc f)

-- helpers to be renamed

cParseIntTypeLength :: [CTypeSpecifier a] -> Maybe Int
cParseIntTypeLength l = case l of
  [CCharType{} ]             -> Just 8
  [CShortType{}]             -> Just 16
  [CShortType{}, CIntType{}] -> Just 16
  -- Not quite right
  []                         -> Just 32
  [CIntType{} ]              -> Just 32
  [CLongType{}]              -> Just 32
  [CLongType{}, CIntType{} ] -> Just 32
  [CLongType{}, CLongType{}] -> Just 64
  [CLongType{}, CLongType{}, CIntType{}] -> Just 64
  _                          -> Nothing

cParseIntType :: [CTypeSpecifier a] -> Maybe Type
cParseIntType l = case l of
  CUnsigType{}  : r -> flip makeIntTy False <$> cParseIntTypeLength r
  CSignedType{} : r -> flip makeIntTy True <$> cParseIntTypeLength r
  r                 -> flip makeIntTy True <$> cParseIntTypeLength r

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

defStruct :: String -> Type -> Circify Type term ()
defStruct n = typedef ("struct " ++ n)

getStruct :: String -> Circify Type term (Maybe Type)
getStruct n = untypedef ("struct " ++ n)

parseBaseTy :: [CTypeSpec] -> Circify Type term (Either String Type)
parseBaseTy ty = case ty of
  [CVoidType{}] -> return $ Right Void
  [CTypeDef (Ident name _ _) _] ->
    maybeToEither ("Missing typedef: " ++ name) <$> untypedef name
  [CBoolType{}  ] -> return $ Right Bool
  [CFloatType{} ] -> return $ Right Float
  [CDoubleType{}] -> return $ Right Double
  [CSUType (CStruct CStructTag mIdent mDecls _ _) _] -> case mDecls of
    -- ident & declarations -> use declarations, store struct type @ ident
    -- ident -> lookup struct type
    -- declarations -> use them
    Just decls -> do
      listMaybeEntryLists <- mapM cSplitDeclaration decls
      let entries = concat <$> sequence listMaybeEntryLists
          s       = Struct . map (\(id, ty, _) -> (id, ty)) <$> entries
      forM_ s
        $ \s -> forM_ mIdent $ \i -> defStruct (identToVarName i) s
      return s
    Nothing -> case mIdent of
      Just ident ->
        let n = identToVarName ident
        in  maybeToEither ("Missing struct definition for " ++ n)
              <$> getStruct n
      Nothing -> do
        text <- liftIO $ nodeText $ head ty
        return $ error $ "struct without declaration or name: " ++ text
  _ | isJust (cParseIntType ty) -> return $ Right $ fromJust $ cParseIntType ty
  _ | isJust (cParseIntType ty) -> return $ Right $ fromJust $ cParseIntType ty
  []                            -> return $ Left "No type!"
  -- Not quite right, need list NodeInfo
  _                             -> do
    text <- liftIO $ nodeText $ head ty
    return $ Left $ unlines ["Unexpected type:", text]

applyDerivations
  :: (Show a) => Type -> [CDerivedDeclarator a] -> Either String Type
applyDerivations ty []       = Right ty
applyDerivations ty (d : ds) = case d of
  CPtrDeclr{} -> applyDerivations (Ptr32 ty) ds
  CArrDeclr _ (CArrSize _ (CConst (CIntConst (CInteger i _ _) _))) _ ->
    applyDerivations (Array (Just $ fromIntegral i) ty) ds
  CArrDeclr _ (CNoArrSize _) _ -> applyDerivations (Array Nothing ty) ds
  _                            -> Left $ "Do not support type " ++ show d

cDeclToType :: CDecl -> Circify Type term (Either String Type)
cDeclToType (CDecl specs _ _) = parseBaseTy $ map specToType specs
cDeclToType _                 = error "nyi"

baseTypeFromSpecs :: [CDeclSpec] -> Circify Type term (Either String Type)
baseTypeFromSpecs all@(elem : rest) = if isTypeQual elem || isAlignSpec elem
  then baseTypeFromSpecs rest
  else parseBaseTy $ mapMaybe typeFromSpec all
baseTypeFromSpecs _ = error "nyi"

nodeText :: (Show a, CNode a) => a -> IO String
nodeText n = fromMaybe ("<Missing text>" ++ show n) <$> nodeTextMaybe n
 where
  nodeTextMaybe :: (Show a, CNode a) => a -> IO (Maybe String)
  nodeTextMaybe n = do
    let pos    = posOfNode $ nodeInfo n
        file   = posFile pos
        lineno = posRow pos
        colno  = posColumn pos
    case lengthOfNode (nodeInfo n) of
      Just len -> do
        lines_ <- lines <$> readFile file
        let rest = unlines $ drop (lineno - 1) lines_
            text = take len $ drop (colno - 1) rest
        return $ Just $ show pos ++ ": " ++ text
      Nothing -> return Nothing

-- A C declaration can be for many variables, each of which may or may not have an init
cSplitDeclaration
  :: CDeclaration NodeInfo
  -> Circify Type term (Either String [(String, Type, Maybe CInit)])
cSplitDeclaration d = case d of
  CDecl specs decls _info -> do
    let firstSpec = head specs
        isTypedefDecl =
          isStorageSpec firstSpec && isTypedef (storageFromSpec firstSpec)
        baseType = if isTypedefDecl then tail specs else specs
    r <- forM decls $ \(Just dec, mInit, _) -> do
      let id      = identFromDeclr dec
          ptrType = derivedFromDeclr dec
      fmap (identToString id, , mInit) <$> ctype baseType ptrType
    return $ sequence r
  _ -> error . ("Unexpected declaration: " ++) <$> liftIO (nodeText d)

-- Other Helpers

fnInfo :: CFunDef -> (FunctionName, [CDecl], CStat)
fnInfo f = (nameFromFunc f, argsFromFunc f, bodyFromFunc f)

nameFromFunc :: CFunDef -> String
nameFromFunc (CFunDef _ decl _ _ _) = identToVarName $ identFromDeclr decl

baseTypeFromFunc :: CFunctionDef a -> [CDeclarationSpecifier a]
baseTypeFromFunc (CFunDef tys _ _ _ _) = tys

bodyFromFunc :: CFunctionDef a -> CStatement a
bodyFromFunc (CFunDef _ _ _ stmt _) = stmt

ptrsFromFunc :: (Show a) => CFunctionDef a -> [CDerivedDeclarator a]
ptrsFromFunc (CFunDef _ decl _ _ _) = case derivedFromDecl decl of
  _ : ptrs -> ptrs
  f        -> error $ unwords ["Expected function declaration but got", show f]

argsFromFunc :: (Show a) => CFunctionDef a -> [CDeclaration a]
argsFromFunc (CFunDef _ decl _ _ _) = case derivedFromDecl decl of
  (CFunDeclr (Right decls) _ _) : _ -> fst decls
  f -> error $ unwords ["Expected function declaration but got", show f]

derivedFromDecl :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDecl (CDeclr _ derived _ _ _) = derived

identToVarName :: Ident -> String
identToVarName (Ident name _ _) = name

specToType :: CDeclarationSpecifier a -> CTypeSpecifier a
specToType spec = case spec of
  CTypeSpec ts -> ts
  _            -> error "Expected type specificer in declaration"

-- General utilities

-- Declarators

identFromDeclr :: CDeclr -> Ident
identFromDeclr d@(CDeclr ids _ _ _ _) =
  fromMaybe (error $ "Expected ident in declarator " ++ show d) ids

derivedFromDeclr :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDeclr (CDeclr _ derived _ _ _) = derived

-- Declaration specifiers

isStorageSpec :: CDeclarationSpecifier a -> Bool
isStorageSpec CStorageSpec{} = True
isStorageSpec _              = False

storageFromSpec :: CDeclarationSpecifier a -> CStorageSpecifier a
storageFromSpec (CStorageSpec spec) = spec
storageFromSpec _                   = error "Expected storage specifier"

typeFromSpec :: (Show a) => CDeclarationSpecifier a -> Maybe (CTypeSpecifier a)
typeFromSpec (CTypeSpec spec) = Just spec
typeFromSpec _                = Nothing

isTypeQual :: CDeclarationSpecifier a -> Bool
isTypeQual CTypeQual{} = True
isTypeQual _           = False

isAlignSpec :: CDeclarationSpecifier a -> Bool
isAlignSpec CAlignSpec{} = True
isAlignSpec _            = False

-- Storage specifiers

isTypedef :: CStorageSpecifier a -> Bool
isTypedef CTypedef{} = True
isTypedef _          = False

module ConsProp.Printer where

import ConsProp.Init
import ConsProp.Apron.Abstract1
import ConsProp.Apron.AbstractMonad
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Control.Monad.State.Strict (liftIO)

initPrinter :: Abstract (CTranslationUnit AbsState) -> String -> IO ()
initPrinter atu name = evalAbstract defaultState $ do
  symT <- liftIO $ getFullSymT name
  initAbstractState Constants symT []
  tu <- atu
  printTU tu

-- Helper function to print the varstate
printSt :: AbsState -> Abstract ()
printSt (State a _) = do
  abs <- a
  abstractPrint(abs)

printTU :: CTranslationUnit AbsState -> Abstract ()
printTU (CTranslUnit [] _) = do
  return ()
printTU (CTranslUnit (ed:eds) a@(State b _)) = do
  printED ed
  printTU (CTranslUnit eds a)

printED :: CExternalDeclaration AbsState -> Abstract ()
printED (CDeclExt cdecl) = do
  printDecl cdecl True
  liftIO $ putStrLn ""
printED (CFDefExt (CFunDef _ (CDeclr (Just (Ident name _ _)) derived _ _ _) _ cstmt st)) = do
  liftIO $ putStrLn ("Function " ++ name ++ ":")
  liftIO $ putStrLn ("Arguments:" ++ (unwords (map printDerivedDecl derived)))
  liftIO $ putStrLn ""
  printStmt cstmt
  liftIO $ putStrLn "End Function\n\n--"
  liftIO $ putStrLn ""
printED _ = do
  return ()

printDerivedDecl :: CDerivedDeclarator AbsState -> String
printDerivedDecl (CFunDeclr (Right (decls, False)) _ _) = unwords (map printArgs decls)
printDerivedDecl _ = ""

printArgs :: CDeclaration AbsState -> String
printArgs (CDecl _ vars _) = " " ++ (unwords (map (\(Just (CDeclr (Just (Ident varName _ _)) _ _ _ _), Nothing, Nothing) -> varName) vars))

-- Function and Statement Level

printDecl :: CDeclaration AbsState -> Bool -> Abstract ()
printDecl (CDecl _ vars st) p = do
  liftIO $ putStrLn ("Decl: " ++ (init (unwords (map printDeclHelper vars))))
  case p of
    True -> printSt st
    False -> return ()

printDeclHelper :: (Maybe (CDeclarator AbsState), Maybe (CInitializer AbsState), Maybe (CExpression AbsState)) -> String
printDeclHelper (Just (CDeclr (Just (Ident varName _ _)) _ _ _ _), Nothing, Nothing) = varName ++ ","
printDeclHelper (Just (CDeclr (Just (Ident varName _ _)) _ _ _ _), (Just (CInitExpr expr _)), Nothing) = varName ++ " = " ++ (printExpr expr) ++ ","
pritnDeclHelper _ = error "Declaration case not implemented"

printStmt :: CStatement AbsState -> Abstract ()
printStmt (CCompound _ cbis _)     = printCBIs cbis
printStmt (CReturn (Just expr) st) = do
  liftIO $ putStrLn ("Return: " ++ printExpr expr)
  printSt st
printStmt (CExpr Nothing _) = return()
printStmt (CExpr (Just expr) st) = do
  liftIO $ putStrLn ("Expr: " ++ printExpr expr)
  printSt st
printStmt (CReturn Nothing st) = do
  liftIO $ putStrLn "Return Void"
  printSt st
-- fstmt is of type Maybe (CStatmenet AbsState)
printStmt (CIf expr tstmt fstmt st) = do
  liftIO $ putStrLn ("If " ++ (printExpr expr) ++ ":\n")
  printStmt tstmt
  liftIO $ putStrLn "Else:\n"
  case fstmt of
    Nothing -> liftIO $ putStrLn "Nothing or not Evaluated\n"
    Just a -> printStmt a
  liftIO $ putStrLn "End If"
  printSt st
-- While Statement
printStmt (CWhile expr stmt False st) = do
  liftIO $ putStrLn ("While " ++ (printExpr expr) ++ ":\n")
  printStmt stmt
  liftIO $ putStrLn "End While"
  printSt st
-- Do-While Statement
printStmt (CWhile expr stmt True st) = do
  liftIO $ putStrLn "Do:\n"
  printStmt stmt
  liftIO $ putStrLn ("While " ++ (printExpr expr) ++ "\n")
  liftIO $ putStrLn "End Do-While"
  printSt st
printStmt (CFor init bound step stmt st) = do
  liftIO $ putStrLn "For:"
  case init of
    Left Nothing           -> liftIO $ putStrLn "Init: None"
    Left (Just expr)       -> liftIO $ putStrLn ("Init: " ++ (printExpr expr))
    Right decl             -> printDecl decl False
  case bound of
    Nothing   -> liftIO $ putStrLn "Bound: None"
    Just expr -> liftIO $ putStrLn ("Bound: " ++ (printExpr expr))
  case step of
    Nothing   -> liftIO $ putStrLn "Step: None"
    Just expr -> liftIO $ putStrLn ("Step: " ++ (printExpr expr))
  liftIO $ putStrLn ""
  printStmt stmt
  liftIO $ putStrLn "End For"
  printSt st
printStmt _ = error "Statement Case Not Implemented"

printCBIs :: [CCompoundBlockItem AbsState] -> Abstract ()
printCBIs [] = return ()
printCBIs ((CBlockStmt stmt):cbis) = do
  printStmt stmt
  liftIO $ putStrLn ""
  printCBIs cbis
printCBIs ((CBlockDecl decl):cbis) = do
  printDecl decl True
  liftIO $ putStrLn ""
  printCBIs cbis
printCBIs _ = error "Nested Function not Implemented"

-- Expression Level

printExpr :: CExpression AbsState -> String
printExpr (CConst (CIntConst cint _)) = show (getCInteger cint)
printExpr (CConst (CStrConst cstr _)) = getCString cstr
printExpr (CVar (Ident n _ _) _) = n
printExpr (CBinary bop expr1 expr2 _) = str1 ++ " " ++ strop ++ " " ++ str2
  where str1  = printExpr expr1
        strop = printBop bop
        str2  = printExpr expr2
printExpr (CUnary unop expr _) =
  case (isPrefixOp unop) of
    True  -> strop ++ "(" ++ str ++ ")"
    False -> "(" ++ str ++ ")" ++ strop
  where strop = printUnop unop
        str   = printExpr expr
printExpr (CAssign assop expr1 expr2 _) = "Assign: " ++ str1 ++ " " ++ strop ++ " " ++ str2
  where str1 = printExpr expr1
        strop = printAssop assop
        str2 = printExpr expr2
printExpr (CIndex expr1 expr2 _) = "Array Item"
printExpr e = error ("Expression Case Not Implemented: " ++ (show e))

isPrefixOp :: CUnaryOp -> Bool
isPrefixOp unop =
  case unop of
    CPostIncOp -> False
    CPostDecOp -> False
    _         -> True

printBop :: CBinaryOp -> String
printBop bop =
  case bop of
    CMulOp -> "*"
    CDivOp -> "/"
    CRmdOp -> "%"
    CAddOp -> "+"
    CSubOp -> "-"
    CShlOp -> "<<"
    CShrOp -> ">>"
    CLeOp  -> "<"
    CGrOp  -> ">"
    CLeqOp -> "<="
    CGeqOp -> ">="
    CEqOp  -> "=="
    CNeqOp -> "!="
    CAndOp -> "&"
    CXorOp -> "^"
    COrOp  -> "|"
    CLndOp -> "&&"
    CLorOp -> "||"

printAssop ::  CAssignOp -> String
printAssop assop =
  case assop of
    CAssignOp -> "="
    CMulAssOp -> "*="
    CDivAssOp -> "/="
    CRmdAssOp -> "%="
    CAddAssOp -> "+="
    CSubAssOp -> "-="
    CShlAssOp -> "<<="
    CShrAssOp -> ">>="
    CAndAssOp -> "&="
    CXorAssOp -> "^="
    COrAssOp  -> "|="

printUnop :: CUnaryOp -> String
printUnop unop =
  case unop of
    CPreIncOp  -> "++"
    CPreDecOp  -> "--"
    CPostIncOp -> "++"
    CPostDecOp -> "--"
    CAdrOp     -> "&"
    CIndOp     -> "*"
    CPlusOp    -> "+"
    CMinOp     -> "-"
    CCompOp    -> "~"
    CNegOp     -> "!"

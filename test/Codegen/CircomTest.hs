{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codegen.CircomTest where
import           AST.Circom
import           BenchUtils
import           Codegen.Circom
import           Codegen.Circom.Constraints (empty)
import           Codegen.Circom.Constraints as Constraints
import           Codegen.Circom.Context
import           Codegen.Circom.ToSmt       (constraintsToSmt)
import           Control.Monad              (unless)
import           Data.Either                (fromLeft, isRight)
import           Data.Field.Galois          (Prime, PrimeField, toP)
import qualified Data.Map.Strict            as Map
import           GHC.TypeLits               (KnownNat)
import           IR.TySmt                   (depth)
import           Parser.Circom              as Parser
import           Utils

signalTerm :: KnownNat k => String -> [Int] -> Term (Prime k)
signalTerm s l = Sig (SigLocal s l)

prime :: Integer
prime = read "113890009193798365449144652900867294558768981710660728242748762258461992583217"

genCtxWithSignals :: KnownNat k => [String] -> Ctx (Prime k)
genCtxWithSignals sigNames = ctxWithEnv (Map.fromList (map (\s -> (s, signalTerm s [])) sigNames)) prime

genCtxWithScalars :: KnownNat k => [(String, Int)] -> Ctx (Prime k)
genCtxWithScalars pairs = ctxWithEnv (Map.fromList (map (\(s, i) -> (s, Scalar $ toP $ fromIntegral i)) pairs)) prime

ctxFromList :: KnownNat k => Map.Map String (Term (Prime k))-> Ctx (Prime k)
ctxFromList l =  ctxWithEnv l prime


circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ genExprTest (genCtxWithSignals []) (NumLit 5) (Scalar 5)
    , genExprTest (genCtxWithSignals []) (BinExpr Shl (NumLit 5) (NumLit 2)) (Scalar 20)
    , genExprTest (genCtxWithSignals []) (BinExpr Div (NumLit 5) (NumLit 1)) (Scalar 5)
    , genExprTest (genCtxWithSignals []) (BinExpr Div (NumLit 5) (NumLit 2)) (Scalar 114)
    , genExprTest (genCtxWithSignals []) (BinExpr IntDiv (NumLit 5) (NumLit 2)) (Scalar 2)
    , genExprTest (genCtxWithSignals ["in"]) (LValue $ Ident "in") (signalTerm "in" [])
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (LValue $ Ident "in") (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 2)], 0))
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (NumLit 5)) (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 6)], 0))
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          )
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Sub (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], -1)], 0)
                          )
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Mul (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               Other
    , genExprTest (genCtxWithSignals [])
               (UnExpr UnPos (ArrayLit [NumLit 5, NumLit 6, NumLit 7]))
               (Scalar 3)
    , genExprTest (genCtxWithSignals []) (UnExpr UnNeg (NumLit 5)) (Scalar (-5))
    , ctxStoreGetTest
        "integer"
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array to integer"
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array"
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdx (LTermIdent "in") 0)
        (Scalar 6)
        (LTermIdent "in")
        (Array [Scalar 6])
    , ctxStoreGetTest
        "struct in array"
        (ctxFromList (Map.fromList [("in", Array [Struct (Map.fromList [("a", Scalar 5)]) empty])]))
        (LTermPin (LTermIdx (LTermIdent "in") 0) "a")
        (Scalar 6)
        (LTermIdent "in")
        (Array [Struct (Map.fromList [("a", Scalar 6)]) empty])
    , genStatementsTest
        "equal"
        (genCtxWithSignals ["a", "b"])
        [Constrain (LValue (Ident "a")) (LValue (Ident "b"))]
        (ctxAddConstraint (genCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -1)], 0)))
    , genStatementsTest
        "twice (assign & constrain)"
        (genCtxWithSignals ["a", "b"])
        [AssignConstrain (Ident "a") (BinExpr Mul (NumLit 2) (LValue (Ident "b")))]
        (ctxAddConstraint (genCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -2)], 0)))
    , genStatementsTest
        "decls of Num2Bits"
        (genCtxWithScalars [("n", 2)])
        [SigDeclaration "in" PublicIn [], SigDeclaration "out" Out [LValue $ Ident "n"]]
        (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            ]
        )
    , genStatementsTest
        "decls of Num2Bits II"
        (genCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        ]
        (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Scalar 0)
            ]
        )
    , genStatementsTest
        "first loop step of Num2Bits"
        (genCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        , If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ]
        (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1) ], 0))
            , ("i", Scalar 1)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , genStatementsTest
        "two loop steps of Num2Bits"
        (genCtxWithScalars [("n", 2)])
        ([ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        ]
        ++ replicate 2
        ( If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ))
        (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , genStatementsTest
        "three loop steps of Num2Bits"
        (genCtxWithScalars [("n", 2)])
        ([ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        ]
        ++ replicate 3
        ( If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ))
        (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , genStatementsTest
        "Num2Bits as while"
        (genCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        , While (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
        , Constrain (LValue (Ident "lc1")) (LValue (Ident "in"))
        ]
        (ctxAddConstraint (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [], 0)
          , (Map.fromList [], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2), (SigLocal "in" [], -1) ], 0)
          )
        )
    , genStatementsTest
        "Num2Bits as for"
        (genCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , For (VarDeclaration "i" [] (Just (NumLit 0))) (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n"))) (Ignore (UnMutExpr PostInc (Ident "i")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            ]
        , Constrain (LValue (Ident "lc1")) (LValue (Ident "in"))
        ]
        (ctxAddConstraint (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [], 0)
          , (Map.fromList [], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2), (SigLocal "in" [], -1) ], 0)
          )
        )
       , genMainTest "test/Code/Circom/bitify4.circom"
         (reverse [ ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [1]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [1]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [2]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [2]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [3]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [3]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1)
                           , (SigForeign "main" [] (SigLocal "out" [1]), 2)
                           , (SigForeign "main" [] (SigLocal "out" [2]), 4)
                           , (SigForeign "main" [] (SigLocal "out" [3]), 8)
                           , (SigForeign "main" [] (SigLocal "in" []), -1)
                           ] , 0)
           )
         ])
       , genMainTest "test/Code/Circom/bitify4-wrapper.circom"
         (reverse [ ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 2)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 4)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 8)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "in" [])), -1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "in" [])), 1)
                           , (SigForeign "main" [] (SigLocal "in" []), -1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [0]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [1]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [2]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [3]), 1)
                           ] , 0)
           )
         ])
       , genMainTestCountOnly "test/Code/Circom/fn.circom" 6 3 6
       , genMainTestCountOnly "test/Code/Circom/multidim.circom" 6 0 7
       , genMainAndConvert "test/Code/Circom/multidim.circom" 7
    ]

genExprTest :: Ctx (Prime 223) -> Expr -> Term (Prime 223) -> BenchTest
genExprTest ctx e t = benchTestCase ("eval " ++ show e) $ do
    let p = genExpr ctx e
    unless (snd p == t) $ error $ "Expected\n\t" ++ show e ++ "\nto evaluate to\n\t" ++ show t ++ "\nbut it evaluated to\n\t" ++ show (snd p) ++ "\n"
    return ()

ctxStoreGetTest :: String -> Ctx (Prime 223) -> LTerm -> Term (Prime 223) -> LTerm -> Term (Prime 223) -> BenchTest
ctxStoreGetTest name ctx sLoc sVal gLoc gVal = benchTestCase ("store/get test: " ++ name) $ do
    let ctx' = ctxStore ctx sLoc sVal
    let gVal' = ctxGet ctx' gLoc
    unless (gVal == gVal') $ error $ "After placing\n\t" ++ show sVal ++ "\nat\n\t" ++ show sLoc ++ "\nin\n\t" ++ show ctx ++"\n, expected\n\t" ++ show gVal ++ "\nat\n\t" ++ show gLoc ++ "\nbut found\n\t" ++ show gVal' ++ "\n"
    return ()

genStatementsTest :: String -> Ctx (Prime 223) -> [Statement] -> Ctx (Prime 223) -> BenchTest
genStatementsTest name ctx s expectCtx' = benchTestCase ("statements: " ++ name) $ do
    let ctx' = genStatements ctx s
    unless (env ctx' == env expectCtx') $
        error $ "Expected\n\t" ++ show s ++ "\nto produce\n\t" ++ show (env expectCtx') ++ "\nbut it produced\n\t" ++ show (env ctx') ++ "\n"
    return ()

genMainTest :: String -> [Constraint (Prime 223)] -> BenchTest
genMainTest path expectedConstraints = benchTestCase ("main gen: " ++ path) $ do
    m <- Parser.loadMain path
    let constraints = equalities (genMain m prime)
    unless (constraints == expectedConstraints) $ error $ "Expected\n\t" ++ show expectedConstraints ++ "\nbut got\n\t" ++ show constraints ++ "\n"
    return ()

genMainTestCountOnly :: String -> Int -> Int -> Int -> BenchTest
genMainTestCountOnly path exCs exPubSigs exPrivSigs = benchTestCase ("signal & constraint counts for circuit at " ++ path) $ do
    m <- Parser.loadMain path
    let constraints = genMain m prime
    let eqs :: [Constraint (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617)] = Constraints.equalities constraints
    unless (length eqs == exCs) $ error $ "Expected " ++ show exCs ++ " constraints, but got " ++ show (length eqs)
    let pubSigs = length (Constraints.public constraints)
    unless (pubSigs == exPubSigs) $
        error $ "Expected " ++ show exPubSigs ++ " public signals, but got " ++ show pubSigs
    let privSigs = length (Constraints.private constraints)
    unless (privSigs == exPrivSigs) $
        error $ "Expected " ++ show exPrivSigs ++ " private signals, but got " ++ show privSigs
    return ()

genMainAndConvert :: String -> Int -> BenchTest
genMainAndConvert path exSignals = benchTestCase ("conversion to Smt for " ++ path) $ do
    m <- Parser.loadMain path
    let constraints :: Constraints (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617) = genMain m prime
    let predicate = constraintsToSmt constraints
    unless (depth predicate == exSignals + 5) $ error $ unwords ["Expected", show exSignals, "signals, got", show (depth predicate - 5)]
    return ()

module Utils where
import           BenchUtils
import           Control.Monad        (forM_, unless)
import qualified Data.Map             as M
import           Targets.SMT.SMTMonad (SMTResult (..))

satTest :: SMTResult -> IO ()
satTest SolverSat{} = return ()
satTest e = error $ unwords ["Expected a SAT result but got", show e]

unsatTest :: SMTResult -> IO ()
unsatTest SolverUnsat{} = return ()
unsatTest e = error $ unwords ["Expected a UNSAT result but got", show e]

vtest :: SMTResult -> M.Map String Double -> IO ()
vtest result expectedVars = case result of
  SolverUnsat -> error "Expected SAT but got UNSAT"
  SolverFailed -> error "Expected SAT but the solver failed"
  SolverSat actualVars -> do
    forM_ (M.toList expectedVars) $ \(expectedVar, expectedVal) -> do
      case M.lookup expectedVar actualVars of
        Nothing -> error $ unwords ["Expected to find"
                                   , show expectedVar
                                   , "in"
                                   , show actualVars
                                   ]
        Just actualVal -> do
          case expectedVal of
            -0.0 -> case actualVal of
                    -0.0 -> return ()
                    _  -> error $ unwords [ "Expected"
                                          , show expectedVar
                                          , "to be negative zero but got"
                                          , show actualVal
                                          ]
            _ -> return ()

          case actualVal of
            -0.0 -> case expectedVal of
                    -0.0 -> return ()
                    _  -> error $ unwords [ "Expected"
                                          , show expectedVar
                                          , "to be"
                                          , show expectedVal
                                          , "but got negative zero"
                                          ]
            _ -> return ()

          unless (actualVal == expectedVal ||
                  (isNaN actualVal && isNaN expectedVal)) $
            error $ unwords ["Expected"
                            , show expectedVar
                            , "to be"
                            , show expectedVal
                            , "but got"
                            , show actualVal
                            ]


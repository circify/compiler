{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module Main where

import           Control.Exception              ( catchJust )
import           Codegen.C.CToR1cs              ( fnToR1cs )
import           Codegen.C                      ( checkFn
                                                , evalFn
                                                )
import           Codegen.LangVal                ( parseToMap
                                                , modelMapToExtMap
                                                )
import qualified Codegen.Circom.Compilation    as Comp
import qualified Codegen.Circom.CompTypes.WitComp
                                               as Wit
import qualified Codegen.Circom.CompTypes      as CompT
import qualified Codegen.Circom.Linking        as Link
import qualified Codegen.Zokrates.Main         as ZGen
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Targets.SMT.TySmtToZ3         as ToZ3
import qualified IR.R1cs                       as R1cs
import qualified IR.R1cs.Opt                   as R1csOpt
import qualified IR.SMT.Opt                    as SmtOpt
import qualified IR.SMT.Opt.Assert             as OptAssert
import qualified IR.SMT.ToPf                   as ToPf
import qualified Parser.Circom.Inputs          as Parse
import qualified Parser.Zokrates               as ZParse
import           Data.Field.Galois              ( toP
                                                , Prime
                                                )
import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import           Parser.C                       ( parseC )
import           Parser.Circom                  ( loadMain )
import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(..)
                                                , exitFailure
                                                )
import           System.IO                      ( IOMode(..)
                                                , Handle
                                                , IOMode
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import qualified System.IO                      ( openFile )
import           System.Console.Docopt
import           System.Directory               ( doesFileExist )
import           System.Process                 ( readProcessWithExitCode )
import           Util.Log
import           Util.Cfg                       ( Cfg )
import qualified Util.Cfg                      as Cfg
                                                ( setFromEnv
                                                , evalCfg
                                                , defaultCfgState
                                                )
import           Util.Show
import qualified Util.ShowMap                  as SMap

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
            (System.IO.openFile path mode)
    $ \e -> do
        putStrLn $ "Could not find file: " ++ path
        return $ error $ "Error: " ++ show e


patterns :: Docopt
patterns = [docopt|
Usage:
  compiler [options] emit-r1cs [options]
  compiler [options] count-terms [options]
  compiler [options] setup [options]
  compiler [options] prove [options]
  compiler [options] verify [options]
  compiler (-h | --help)
  compiler [options] c-check <fn-name> <path>
  compiler [options] c-emit-r1cs <fn-name> <path>
  compiler [options] c-setup <fn-name> <path>
  compiler [options] c-prove <fn-name> <path>
  compiler [options] c-eval <fn-name> <path>
  compiler [options] c-check-setup <fn-name> <path>
  compiler [options] c-check-emit-r1cs <fn-name> <path>
  compiler [options] c-check-prove <fn-name> <path>
  compiler [options] zokrates-parse <path>
  compiler [options] zokrates-emit-r1cs <fn-name> <path>
  compiler [options] zokrates-setup <fn-name> <path>
  compiler [options] zokrates-prove <fn-name> <path>

Options:
  -h, --help         Display this message
  --circom <path>    Read the circom circuit from this path [default: main.circom]
  -C <path>          Write the R1CS circuit to this path [default: C]
  -V <path>          Write the verifier key to this path [default: vk]
  -P <path>          Write the prover key to this path [default: pk]
  -i <path>          Read all inputs from this path [default: i]
  -x <path>          Write the public input to this path [default: x]
  -w <path>          Write the the auxiliary input to this path [default: w]
  -p <path>          Write/Read the proof at this path [default: pf]
  --libsnark <path>  Location of the libsnark binary [default: libsnark-frontend/build/src/main]
  --json             Whether to write json

Commands:
  prove            Run the prover
  verify           Run the verifier
  setup            Run the setup
  emit-r1cs        Emit R1CS
  count-terms      Compile at the fn-level only
  c-check          Check a C function using an SMT solver
  c-eval           Evaluate C function & print model
  c-emit-r1cs      Convert a C function to R1CS
  c-setup          Run setup for a C function
  c-prove          Write proof for a C function
  c-check-setup    Setup a proof of bug
  c-check-prove    Find a bug and prove its presense
|]

getArgOrExit :: Arguments -> Option -> Cfg String
getArgOrExit a o = liftIO $ getArgOrExitWith patterns a o

getExistingFilePath :: Arguments -> Option -> Cfg FilePath
getExistingFilePath a o = do
  p <- getArgOrExit a o
  e <- liftIO $ doesFileExist p
  if e then return p else error $ "File " ++ show p ++ " does not exist"

checkProcess :: FilePath -> [String] -> String -> IO ()
checkProcess pgm args input = do
  putStrLn $ "Running: " ++ pgm ++ " " ++ unwords args
  (code, stdout, stderr) <- readProcessWithExitCode pgm args input
  unless (code == ExitSuccess) $ do
    putStrLn "STDOUT"
    putStrLn stdout
    putStrLn "END STDOUT"
    putStrLn "STDERR"
    putStrLn stderr
    putStrLn "END STDERR"
    putStrLn $ pgm ++ " return code " ++ show code ++ ", see above."
    exitFailure

-- libsnark functions
runSetup :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runSetup libsnark circuitPath pkPath vkPath = do
  checkProcess libsnark
               ["setup", "-V", vkPath, "-P", pkPath, "-C", circuitPath]
               ""
  return ()

runProve
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO ()
runProve libsnark pkPath vkPath xPath wPath pfPath = checkProcess
  libsnark
  ["prove", "-V", vkPath, "-P", pkPath, "-x", xPath, "-w", wPath, "-p", pfPath]
  ""

runVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runVerify libsnark vkPath xPath pfPath =
  checkProcess libsnark ["verify", "-V", vkPath, "-x", xPath, "-p", pfPath] ""

order :: Integer
order =
  21888242871839275222246405745257275088548364400416034343698204186575808495617
type Order
  = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- order = 17
-- type Order = 17
-- type OrderCtx = Ctx Order

cmdEmitR1cs :: Bool -> FilePath -> FilePath -> Cfg ()
cmdEmitR1cs asJson circomPath r1csPath = do
  liftIO $ print "Loading circuit"
  m    <- liftIO $ loadMain circomPath
  r1cs <- evalLog $ (Link.linkMain @Order m >>= R1csOpt.opt)
  liftIO $ do
    putStrLn $ R1cs.r1csStats r1cs
    --putStrLn $ R1cs.r1csShow r1cs
    R1cs.writeToR1csFile asJson r1cs r1csPath

cmdCountTerms :: FilePath -> Cfg ()
cmdCountTerms circomPath = do
  m <- liftIO $ loadMain circomPath
  c <- evalLog $ Comp.compMainWitCtx @Order m
  liftIO $ do
    print $ count c + sum (Map.map count $ CompT.cache c)
    print c
  where count = Wit.nSmtNodes . CompT.baseCtx


cmdSetup :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Cfg ()
cmdSetup libsnark circomPath r1csPath pkPath vkPath = do
  cmdEmitR1cs False circomPath r1csPath
  liftIO $ runSetup libsnark r1csPath pkPath vkPath

cmdProve
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Cfg ()
cmdProve libsnark pkPath vkPath inPath xPath wPath pfPath circomPath = do
  m             <- liftIO $ loadMain circomPath
  inputFile     <- liftIO $ openFile inPath ReadMode
  inputsSignals <- liftIO $ Parse.parseSignalsFromFile (Proxy @Order) inputFile
  allSignals <- evalLog $ Link.computeWitnesses (Proxy @Order) m inputsSignals
  r1cs          <- evalLog (Link.linkMain @Order m >>= R1csOpt.opt)
  let getOr m_ k =
        Maybe.fromMaybe (error $ "Missing sig: " ++ show k) $ m_ Map.!? k
  let getOrI m_ k =
        Maybe.fromMaybe (error $ "Missing sig num: " ++ show k) $ m_ IntMap.!? k
  let lookupSignalVal :: Int -> Prime Order =
        toP . getOr allSignals . head . getOrI (Link.numSigs r1cs)
  liftIO $ R1cs.emitAssignment
    (map lookupSignalVal [2 .. (1 + Link.nPublicInputs r1cs)])
    xPath
  liftIO $ R1cs.emitAssignment
    (map lookupSignalVal
         [(2 + Link.nPublicInputs r1cs) .. (Link.nextSigNum r1cs - 1)]
    )
    wPath
  liftIO $ runProve libsnark pkPath vkPath xPath wPath pfPath

cmdVerify :: FilePath -> FilePath -> FilePath -> FilePath -> Cfg ()
cmdVerify = ((.) . (.) . (.) . (.)) liftIO runVerify

cmdCCheck :: String -> FilePath -> Cfg ()
cmdCCheck name path = do
  tu  <- liftIO $ parseC path
  res <- evalLog $ do
    res <- checkFn tu name
    logIf "time::z3" $ "Z3 Time: " ++ show (ToZ3.time res)
    return res
  if ToZ3.sat res
    then do
      liftIO $ putStrLn "Bug!"
      forM_ (Map.toList $ ToZ3.model res)
        $ \(k, v) -> liftIO $ putStrLn $ unwords [k, ":", show v]
    else liftIO $ putStrLn "No bug!"

cmdCEval :: String -> FilePath -> Cfg ()
cmdCEval name path = do
  tu <- liftIO $ parseC path
  r  <- evalLog $ evalFn False tu name
  forM_ (Map.toList r) $ \(k, v) -> liftIO $ putStrLn $ unwords [k, ":", show v]

cmdCEmitR1cs :: Bool -> Bool -> String -> FilePath -> FilePath -> Cfg ()
cmdCEmitR1cs findBugs asJson fnName cPath r1csPath = do
  tu   <- liftIO $ parseC cPath
  r1cs <- evalLog $ fnToR1cs @Order findBugs Nothing tu fnName
  liftIO $ R1cs.writeToR1csFile asJson r1cs r1csPath

cmdCSetup
  :: Bool
  -> FilePath
  -> String
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Cfg ()
cmdCSetup findBugs libsnark fnName cPath r1csPath pkPath vkPath = do
  cmdCEmitR1cs findBugs False fnName cPath r1csPath
  liftIO $ runSetup libsnark r1csPath pkPath vkPath

cmdCProve
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> String
  -> FilePath
  -> Cfg ()
cmdCProve libsnark pkPath vkPath inPath xPath wPath pfPath fnName cPath = do
  tu    <- liftIO $ parseC cPath
  inMap <- liftIO $ parseToMap <$> readFile inPath
  r1cs  <- evalLog $ fnToR1cs @Order False (Just inMap) tu fnName
  case R1cs.r1csCheck r1cs of
    Right _ -> return ()
    Left  e -> liftIO $ do
      putStrLn e
      exitFailure
  liftIO $ do
    R1cs.r1csWriteAssignments r1cs xPath wPath
    runProve libsnark pkPath vkPath xPath wPath pfPath

cmdCCheckProve
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> String
  -> FilePath
  -> Cfg ()
cmdCCheckProve libsnark pkPath vkPath _inPath xPath wPath pfPath fnName cPath =
  do
    tu  <- liftIO $ parseC cPath
    res <- evalLog $ checkFn tu fnName
    unless (ToZ3.sat res) $ error "No bug!"
    let r     = ToZ3.model res
        inMap = modelMapToExtMap r
    liftIO $ forM_ (Map.toList r) $ \(k, v) ->
      liftIO $ putStrLn $ unwords [k, ":", show v]
    r1cs <- evalLog $ fnToR1cs @Order True (Just inMap) tu fnName
    case R1cs.r1csCheck r1cs of
      Right _ -> return ()
      Left  e -> liftIO $ do
        putStrLn e
        exitFailure
    liftIO $ do
      R1cs.r1csWriteAssignments r1cs xPath wPath
      runProve libsnark pkPath vkPath xPath wPath pfPath

cmdZokratesParse :: FilePath -> Cfg ()
cmdZokratesParse path = do
  ast <- liftIO $ ZParse.loadFilesRecursively path
  liftIO $ putStrLn $ pShow ast

cmdZokratesEmitR1cs :: FilePath -> String -> Bool -> FilePath -> Cfg ()
cmdZokratesEmitR1cs path fnName asJson r1csPath = do
  files <- liftIO $ ZParse.loadFilesRecursively path
  r1cs  <- evalLog $ do
    assertState <- ZGen.run @Order path fnName files Nothing
    --liftIO $ putStrLn $ pShow assertState
    newSmt      <- SmtOpt.opt SMap.empty assertState
    r           <- ToPf.toPf @Order (OptAssert._vals newSmt)
                                    (OptAssert._public newSmt)
                                    SMap.empty
                                    (OptAssert.listAssertions newSmt)
    R1csOpt.opt r
  liftIO $ do
    putStrLn $ R1cs.r1csStats r1cs
    R1cs.writeToR1csFile asJson r1cs r1csPath

cmdZokratesSetup
  :: FilePath
  -> FilePath
  -> String
  -> FilePath
  -> FilePath
  -> FilePath
  -> Cfg ()
cmdZokratesSetup libsnarkPath path fnName r1csPath pkPath vkPath = do
  cmdZokratesEmitR1cs path fnName False r1csPath
  liftIO $ runSetup libsnarkPath r1csPath pkPath vkPath

cmdZokratesProve
  :: FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> String
  -> FilePath
  -> Cfg ()
cmdZokratesProve libsnark pkPath vkPath inPath xPath wPath pfPath fnName path =
  do
    files <- liftIO $ ZParse.loadFilesRecursively path
    inMap <- liftIO $ parseToMap <$> readFile inPath
    r1cs  <- evalLog $ do
      assertState <- ZGen.run @Order path fnName files (Just inMap)
      liftIO $ putStrLn $ pShow assertState
      newSmt <- SmtOpt.opt SMap.empty assertState
      liftIO $ putStrLn $ pShow newSmt
      r <- ToPf.toPf @Order (OptAssert._vals newSmt)
                            (OptAssert._public newSmt)
                            SMap.empty
                            (OptAssert.listAssertions newSmt)
      R1csOpt.opt r
    case R1cs.r1csCheck r1cs of
      Right _ -> return ()
      Left  e -> liftIO $ do
        putStrLn e
        exitFailure
    liftIO $ do
      R1cs.r1csWriteAssignments r1cs xPath wPath
      runProve libsnark pkPath vkPath xPath wPath pfPath

defaultR1cs :: String
defaultR1cs = "C"

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  let
    cmd :: Cfg () = case True of
      _ | args `isPresent` command "emit-r1cs" -> do
        circomPath <- args `getExistingFilePath` longOption "circom"
        let asJson = args `isPresent` longOption "json"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        cmdEmitR1cs asJson circomPath r1csPath
      _ | args `isPresent` command "count-terms" -> do
        circomPath <- args `getExistingFilePath` longOption "circom"
        cmdCountTerms circomPath
      _ | args `isPresent` command "setup" -> do
        libsnark   <- args `getExistingFilePath` longOption "libsnark"
        circomPath <- args `getExistingFilePath` longOption "circom"
        r1csPath   <- args `getArgOrExit` shortOption 'C'
        pkPath     <- args `getArgOrExit` shortOption 'P'
        vkPath     <- args `getArgOrExit` shortOption 'V'
        cmdSetup libsnark circomPath r1csPath pkPath vkPath
      _ | args `isPresent` command "prove" -> do
        libsnark   <- args `getExistingFilePath` longOption "libsnark"
        circomPath <- args `getExistingFilePath` longOption "circom"
        pkPath     <- args `getExistingFilePath` shortOption 'P'
        vkPath     <- args `getExistingFilePath` shortOption 'V'
        inPath     <- args `getExistingFilePath` shortOption 'i'
        xPath      <- args `getArgOrExit` shortOption 'x'
        wPath      <- args `getArgOrExit` shortOption 'w'
        pfPath     <- args `getArgOrExit` shortOption 'p'
        cmdProve libsnark pkPath vkPath inPath xPath wPath pfPath circomPath
      _ | args `isPresent` command "verify" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        vkPath   <- args `getExistingFilePath` shortOption 'V'
        xPath    <- args `getExistingFilePath` shortOption 'x'
        pfPath   <- args `getExistingFilePath` shortOption 'p'
        cmdVerify libsnark vkPath xPath pfPath
      _ | args `isPresent` command "c-check" -> do
        fnName <- args `getArgOrExit` argument "fn-name"
        path   <- args `getExistingFilePath` argument "path"
        cmdCCheck fnName path
      _ | args `isPresent` command "c-eval" -> do
        fnName <- args `getArgOrExit` argument "fn-name"
        path   <- args `getExistingFilePath` argument "path"
        cmdCEval fnName path
      _ | args `isPresent` command "c-emit-r1cs" -> do
        let asJson = args `isPresent` longOption "json"
        fnName   <- args `getArgOrExit` argument "fn-name"
        path     <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        cmdCEmitR1cs False asJson fnName path r1csPath
      _ | args `isPresent` command "c-setup" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        pkPath   <- args `getArgOrExit` shortOption 'P'
        vkPath   <- args `getArgOrExit` shortOption 'V'
        cmdCSetup False libsnark fnName cPath r1csPath pkPath vkPath
      _ | args `isPresent` command "c-prove" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        pkPath   <- args `getExistingFilePath` shortOption 'P'
        vkPath   <- args `getExistingFilePath` shortOption 'V'
        inPath   <- args `getExistingFilePath` shortOption 'i'
        xPath    <- args `getArgOrExit` shortOption 'x'
        wPath    <- args `getArgOrExit` shortOption 'w'
        pfPath   <- args `getArgOrExit` shortOption 'p'
        cmdCProve libsnark pkPath vkPath inPath xPath wPath pfPath fnName cPath
      _ | args `isPresent` command "c-check-emit-r1cs" -> do
        let asJson = args `isPresent` longOption "json"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        cmdCEmitR1cs True asJson fnName cPath r1csPath
      _ | args `isPresent` command "c-check-setup" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        pkPath   <- args `getArgOrExit` shortOption 'P'
        vkPath   <- args `getArgOrExit` shortOption 'V'
        cmdCSetup True libsnark fnName cPath r1csPath pkPath vkPath
      _ | args `isPresent` command "c-check-prove" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        pkPath   <- args `getExistingFilePath` shortOption 'P'
        vkPath   <- args `getExistingFilePath` shortOption 'V'
        inPath   <- args `getArgOrExit` shortOption 'i'
        xPath    <- args `getArgOrExit` shortOption 'x'
        wPath    <- args `getArgOrExit` shortOption 'w'
        pfPath   <- args `getArgOrExit` shortOption 'p'
        cmdCCheckProve libsnark
                       pkPath
                       vkPath
                       inPath
                       xPath
                       wPath
                       pfPath
                       fnName
                       cPath
      _ | args `isPresent` command "zokrates-parse" -> do
        path <- args `getExistingFilePath` argument "path"
        cmdZokratesParse path
      _ | args `isPresent` command "zokrates-emit-r1cs" -> do
        fnName   <- args `getArgOrExit` argument "fn-name"
        path     <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        let asJson = args `isPresent` longOption "json"
        cmdZokratesEmitR1cs path fnName asJson r1csPath
      _ | args `isPresent` command "zokrates-setup" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        path     <- args `getExistingFilePath` argument "path"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        pkPath   <- args `getArgOrExit` shortOption 'P'
        vkPath   <- args `getArgOrExit` shortOption 'V'
        cmdZokratesSetup libsnark path fnName r1csPath pkPath vkPath
      _ | args `isPresent` command "zokrates-prove" -> do
        libsnark <- args `getExistingFilePath` longOption "libsnark"
        fnName   <- args `getArgOrExit` argument "fn-name"
        cPath    <- args `getExistingFilePath` argument "path"
        pkPath   <- args `getExistingFilePath` shortOption 'P'
        vkPath   <- args `getExistingFilePath` shortOption 'V'
        inPath   <- args `getArgOrExit` shortOption 'i'
        xPath    <- args `getArgOrExit` shortOption 'x'
        wPath    <- args `getArgOrExit` shortOption 'w'
        pfPath   <- args `getArgOrExit` shortOption 'p'
        cmdZokratesProve libsnark
                         pkPath
                         vkPath
                         inPath
                         xPath
                         wPath
                         pfPath
                         fnName
                         cPath
      _ -> liftIO $ exitWithUsageMessage patterns "Missing command!"
  cfg <- Cfg.setFromEnv Cfg.defaultCfgState
  Cfg.evalCfg cmd cfg

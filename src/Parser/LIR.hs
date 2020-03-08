{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad   (foldM, forM_, unless, when)
import           Data.Aeson
import qualified Data.Map        as M
import           Data.Maybe      (fromJust, isJust, isNothing)
import           Data.Text       hiding (length, null, unwords, zip)
import           Prelude         hiding (id)
import           Static.Regalloc

parseRegAlloc :: FilePath -> IO (Maybe [Graph])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/moveGroup.json"
  case r of
    Just graphs -> do
      let regs = makeRegallocMap graphs
          befores = beforeRegalloc regs
          afters  = afterRegalloc regs
      forM_ (M.keys befores) $ \k -> do
        let lirBefore = blocks $ befores M.! k
            lirAfter  = blocks $ afters M.! k
        forM_ (zip lirBefore lirAfter) $ \(bblock, ablock) -> do
          print "BLOCK BOUNDARY"
          let beforeNodes = makeNodeMap $ nodes bblock
              afterNodes  = nodes ablock
          forM_ afterNodes $ \node -> do
            print "-----------------------------------"
            let nodeid = id node
            case operation node of
              LMoveGroupOp moves -> forM_ moves print
              LOp "Phi"          -> do
                let (Just (outreg, inregs)) = getPhi node
                putStrLn $ unwords [show outreg, "is phi of", show inregs]
              _                  -> do
                print $ code $ operation node
                let beforeNode = beforeNodes M.! nodeid
                case defs node of
                  [def] -> do
                     let vdef = getVirtualLDefs def
                         rdef = getRealLDefs def
                     case rdef of
                       Just rname -> putStrLn $ unwords [show vdef, "is", show rname]
                       _          -> putStrLn $ unwords [show vdef, "is", show def]
                  [] -> return ()
                  _  -> error "Unexpected number of definitions"
                let ops  = zip (operands beforeNode) (operands node)
                unless (null ops) $ print "with operands"
                forM_ ops $ \(bop, aop) ->
                  case getVirtualAllocation bop of
                    Nothing -> return ()
                    Just vr -> case getRealAllocation aop of
                      Nothing -> putStrLn $ unwords [show vr, "is", show aop]
                      Just rr -> putStrLn $ unwords [show vr, "is", show rr]
    Nothing     -> print "Failed"

{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad   (foldM, forM_, unless, when)
import           Data.Aeson
import qualified Data.Map        as M
import           Data.Maybe      (fromJust, isJust, isNothing)
import           Data.Text       hiding (length, unwords, zip)
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
          beforeSize = M.size befores
          afterSize  = M.size afters
      unless (M.keys befores == M.keys afters) $
        error "Blocks should have the same numbering before and after regalloc"
      forM_ (M.keys befores) $ \k -> do
        let lirBefore = blocks $ befores M.! k
            lirAfter  = blocks $ afters M.! k
        unless (length lirBefore == length lirAfter) $
          error "There should be the same number of blocks before and after regalloc"
        forM_ (zip lirBefore lirAfter) $ \(beforeBlock, afterBlock) -> do
          let beforeNodes = nodes beforeBlock
              afterNodes  = nodes afterBlock
          when (length beforeNodes == length afterNodes) $ do

            forM_ (zip beforeNodes afterNodes) $ \(b, a) -> do
              let beforeOps = operands b
                  afterOps  = operands a
              unless (length beforeOps == length afterOps) $ error "Lost operands"
              rs <- foldM (\regs (bo, ao) -> do
                      let bv = getVirtualAllocation bo
                          av = getRealAllocation ao
                      return $ if isJust bv && isJust av
                               then addAssignment (fromJust bv) (fromJust av) regs
                               else regs
                          ) M.empty $ zip beforeOps afterOps
              forM_ (M.toList rs) $ (\(r, v) ->
                putStrLn $ unwords $ ["Real is", show r, "virt is", show v] )
              print "--------------------------------------------------------"


              -- let beforeDefs = defs b
              --     afterDefs  = defs a
              -- unless (length beforeDefs == length afterDefs) $ error "Lost temps"
              -- forM_ (zip beforeDefs afterDefs) $ \(bd, ad) -> do
              --   print $ getVirtualLDefs bd
              --   print $ getRealLDefs ad
              -- let beforeTemps = temps b
              --     afterTemps  = temps a
              -- unless (length beforeTemps == length afterTemps) $ error "Lost temps"
              -- forM_ (zip beforeTemps afterTemps) $ \(bt, at) -> do
              --   print $ getVirtualLDefs bt
              --   print $ getRealLDefs at

    Nothing     -> print "Failed"

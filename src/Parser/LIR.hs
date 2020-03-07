{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (forM_, unless, when)
import           Data.Aeson
import qualified Data.Map      as M
import           Data.Text     hiding (length, zip)

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
            putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            forM_ beforeNodes $ print . getVirtualOperands
            forM_ beforeNodes $ print . getVirtualDefs
            forM_ beforeNodes $ print . getVirtualTemps
            putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            forM_ beforeNodes $ print . getRealOperands
            forM_ beforeNodes $ print . getRealDefs
            forM_ beforeNodes $ print . getRealTemps
    Nothing     -> print "Failed"

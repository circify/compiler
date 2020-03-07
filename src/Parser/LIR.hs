{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (forM_, unless)
import           Data.Aeson
import qualified Data.Map      as M
import           Data.Text     hiding (length)

parseRegAlloc :: FilePath -> IO (Maybe [Graph])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/graphIds1.json"
  case r of
    Just graphs -> do
      let regs = makeRegallocMap graphs
          beforeSize = M.size $ beforeRegalloc regs
          afterSize  = M.size $ afterRegalloc regs
      unless (beforeSize == afterSize) $
        error "Different number of blocks before and after regalloc"
    Nothing     -> print "Failed"

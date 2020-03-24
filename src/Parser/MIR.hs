{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.MIR where
import           AST.LICM
import           AST.MIR
import           Control.Monad (foldM, forM_, unless, when)
import           Data.Aeson
import qualified Data.Map      as M
import           Data.Maybe    (fromJust, isJust, isNothing)
import           Data.Text     hiding (length, null, unwords, zip)
import           Prelude       hiding (id)

parseLICM :: FilePath -> IO (Maybe [OptGraph])
parseLICM name = decodeFileStrict name

printLICM :: IO ()
printLICM = do
  r <- parseLICM "examples/mir-correct.json"
  case r of
    Just graphs -> do
      let regs = makeLICMMap graphs
          befores = beforeLICM regs
          afters  = afterLICM regs
      forM_ (M.keys befores) $ \k ->
        when (M.member k afters) $ do
          let before = befores M.! k
              after  = afters M.! k
              beforeBlocks = blocks before
              afterBlocks  = blocks after
              beforeInstrs = instrs $ beforeBlocks !! 0
              afterInstrs = instrs $ afterBlocks !! 0
          print $ beforeInstrs == afterInstrs

    Nothing -> error "ERROR PARSING"

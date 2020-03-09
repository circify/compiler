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
import           Static.Kildall
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
        let lirBefore = befores M.! k
            lirAfter  = afters M.! k
            worklist  = initList [lirBefore, lirAfter]
            state     = initState [lirBefore, lirAfter]
        print $ kildall worklist state [lirBefore, lirAfter]
        -- forM_ (zip lirBefore lirAfter) $ \(bblock, ablock) -> do
        --   print "BLOCK BOUNDARY"
        --   let beforeNodes = makeNodeMap $ nodes bblock
        --       afterNodes  = nodes ablock
        --   forM_ afterNodes $ \node -> do
        --     print "-----------------------------------"
        --     let nodeid = id node
        --         bnode  = beforeNodes M.! nodeid
        --     if (M.member nodeid beforeNodes)
        --     then do
        --       print $ getNodeInfo bnode node
        --     else do
        --       print $ getNodeMoveInfo node
    Nothing     -> print "Failed"

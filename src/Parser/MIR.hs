{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.MIR where
import           AST.LICM
import           AST.MIR
import           Control.Monad     (foldM, forM, forM_, unless, void, when)
import           Data.Aeson
import qualified Data.Map          as M
import           Data.Maybe        (fromJust, isJust, isNothing)
import           Data.Text         hiding (length, null, unwords, zip)
import           Prelude           hiding (id)
import           Static.KildallMIR
import           Static.LICM

parseLICM :: FilePath -> IO (Maybe [OptGraph])
parseLICM name = decodeFileStrict name

printLICM :: IO ()
printLICM = do
  r <- parseLICM "examples/mir5.json"
  case r of
    Just graphs -> do
      let regs = makeLICMMap graphs
          befores = beforeLICM regs
          afters  = afterLICM regs
      unless (befores == afters) $ do
        -- go through the graphIds
        forM_ (M.keys befores) $ \k -> do
          when (M.member k afters) $ do
            -- Get the MIR graphs
            let beforeMIR = befores M.! k
                afterMIR  = afters M.! k
            -- Analyze the befores
                beforeList  = initList beforeMIR
                beforeState = initState beforeMIR
            beforeDefs <- kildall beforeList beforeState beforeMIR
            -- Analyze the afters
            let afterList  = initList afterMIR
                afterState = initState afterMIR
            afterDefs <- kildall afterList afterState afterMIR
            putStrLn $ unwords $ ["Checking", show k]
            void $ check beforeMIR beforeDefs

    --   afterResults <-  forM afters $ \after -> do
      --     let worklist = initList after
      --         state    = initState after
      --     kildall worklist state after
      --   unless (beforeResults == afterResults) $ do
      --     -- Compare the results for each graph
      --     forM_ (M.keys beforeResults) $ \k -> do
      --       liftIO $ print k
            -- when (M.member k afterResults) $ do
            --   beforeMap <- depmap $ foldM meet Start $ storeMap $ beforeResults M.! k
            --   afterMap  <- depmap $ foldM meet Start $ storeMap $ afterResults M.! k
            --   print $ beforeMap == afterMap
            --   forM_ (M.keys beforeMap) $ \k -> error ""
                -- if M.member k afterMap
                -- then do
                --   let beforeElem = beforeMap M.! k
                --      afterElem   = afterMap M.! k
                --   unless (beforeElem == afterElem) $ do
                --     putStrLn "ERROR ELEMENTS"
                --     print k
                --     print beforeElem
                -- else do
                --   putStrLn "ERROR MISSING"
                --   print k



      -- forM_ (M.keys befores) $ \k ->
      --   when (M.member k afters) $ do
      --     let before = befores M.! k
      --         after  = afters M.! k
      --         beforeBlocks = blocks before
      --         afterBlocks  = blocks after
      --     forM_ (zip beforeBlocks afterBlocks) $ \(b, a) ->
      --       unless (instrs b == instrs a) $ do
      --         putStrLn "=========NOT EQUAL. Before then after=========="
      --         forM_ (instrs b) $ putStrLn . prettyNode
      --         putStrLn "-----------------------------------------"
      --         forM_ (instrs a) $ putStrLn . prettyNode

    Nothing -> error "ERROR PARSING"

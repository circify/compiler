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

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseLICM "examples/mir.json"
  case r of
    Just graphs -> do
      let regs = makeLICMMap graphs
          befores = beforeLICM regs
          afters  = afterLICM regs
      forM_ (M.keys befores) $ \k -> do
        let mirBefore = befores M.! k
            mirAfter  = afters M.! k
        print mirBefore
        print mirAfter

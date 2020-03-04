{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Data.Aeson
import           Data.Text

parseRegAlloc :: FilePath -> IO (Maybe [LAllocation])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/test2.json"
  print r

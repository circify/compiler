{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.Regalloc
import           Data.Aeson
import           Data.Text

parseRegAlloc :: FilePath -> IO (Maybe AllComparisons)
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/test.json"
  print r

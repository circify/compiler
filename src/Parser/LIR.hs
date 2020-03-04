{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Data.Aeson
import           Data.Text

parseRegAlloc :: FilePath -> IO (Maybe [Comparison])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/ion-26744.json"
  print r

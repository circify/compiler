{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Text     hiding (length)

parseRegAlloc :: FilePath -> IO (Maybe [Graph])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/graphIds1.json"
  case r of
    Just graphs -> print $ makeRegallocMap graphs
    Nothing     -> print "Failed"

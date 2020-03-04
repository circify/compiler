{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           AST.LIR
import           AST.Regalloc
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Text     hiding (length)

parseRegAlloc :: FilePath -> IO (Maybe [Comparison])
parseRegAlloc name = decodeFileStrict name

printRegAlloc :: IO ()
printRegAlloc = do
  r <- parseRegAlloc "examples/ion-26744.json"
  case r of
    Just comps -> forM_ comps $ \comp -> do
                    print $ makeBlockMap $ beforeRegisterAllocation comp
                    print $ makeBlockMap $ afterRegisterAllocation comp

    Nothing -> print "Failed"

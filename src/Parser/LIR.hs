{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           Data.Aeson
import           GHC.Generics

data Block = Block { blockNumber  :: Int
                   , instructions :: [String]
                   } deriving (Generic, Show)

data AST = AST [Block] deriving (Generic, Show)

instance ToJSON AST where
instance ToJSON Block where
instance FromJSON AST where
instance FromJSON Block where

parseAST :: FilePath -> IO (Maybe AST)
parseAST name = decodeFileStrict name

printAST :: IO ()
printAST = do
  r <- parseAST "examples/data.json"
  print r

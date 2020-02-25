{-# LANGUAGE DeriveGeneric #-}
module Parser.LIR where
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Block = Block { blockNumber  :: Int
                   , instructions :: [Text]
                   } deriving (Generic, Show)

data Comparison = Comparison {
          beforeRegisterAllocation :: [Block]
        , afterRegisterAllocation  :: [Block]
        } deriving (Generic, Show)

data AllComparisons = AllComparisons [Comparison] deriving (Generic, Show)

instance ToJSON Block where
instance FromJSON Block where
instance ToJSON Comparison where
instance FromJSON Comparison where
instance ToJSON AllComparisons where
instance FromJSON AllComparisons where

parseAST :: FilePath -> IO (Maybe AllComparisons)
parseAST name = decodeFileStrict name

printAST :: IO ()
printAST = do
  r <- parseAST "examples/test.json"
  print r

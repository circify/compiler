{-# LANGUAGE DeriveGeneric #-}
module AST.Regalloc where
import           AST.LIR
import           Data.Aeson
import           GHC.Generics

-- data Comparison = Comparison {
--           beforeRegisterAllocation :: LIR
--         , afterRegisterAllocation  :: LIR
--         } deriving (Generic, Show)

-- data AllComparisons = AllComparisons [Comparison] deriving (Generic, Show)

-- instance ToJSON Comparison where
-- instance FromJSON Comparison where
-- instance ToJSON AllComparisons where
-- instance FromJSON AllComparisons where

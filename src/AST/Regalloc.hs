{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AST.Regalloc where
import           AST.LIR
import           Control.Monad.State.Strict (liftIO, unless)
import           Data.Aeson
import           Data.Text                  hiding (unwords)
import           GHC.Generics

data Comparison = Comparison {
          beforeRegisterAllocation :: LIR
        , afterRegisterAllocation  :: LIR
        } deriving (Generic, Show)

instance FromJSON Comparison where
    parseJSON = withObject "comparison" $ \o -> do
      (s1 :: Text) <- o .: ("name" :: Text)
      before <- o .: ("lir" :: Text)
      after <- o .: ("lir" :: Text)
      (s2 :: Text) <- o .: ("name" :: Text)
      return $ Comparison before after


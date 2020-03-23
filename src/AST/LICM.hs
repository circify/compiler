{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AST.LICM where
import           AST.MIR
import           Data.Aeson
import qualified Data.Map    as M
import           Data.Text   hiding (foldl, unwords)
import           Data.Word
import           Debug.Trace

data LICMMap = LICMMap { beforeLICM :: M.Map Word32 MIR
                       , afterLICM  :: M.Map Word32 MIR
                       }
                 deriving (Show)

emptyLICMMap = LICMMap M.empty M.empty

makeLICMMap :: [OptGraph] -> LICMMap
makeLICMMap graphs =
  foldl (\(LICMMap before after) (OptGraph idx when lir) ->
           LICMMap (if when == BeforeLICM then M.insert idx lir before else before)
                   (if when == AfterLICM then M.insert idx lir after else after)
        ) emptyLICMMap graphs

data OptGraph = OptGraph Word32 When MIR
              deriving (Show)

data When = BeforeLICM | AfterLICM
          deriving (Eq, Ord, Show)

instance FromJSON OptGraph where
    parseJSON = withObject "comparison" $ \o -> do
      gid <- o .: ("graphId" :: Text)
      (when :: Text) <- o .: ("pass" :: Text)
      let when' = case when of
                    "beforeLICM" -> BeforeLICM
                    "afterLICM"  -> AfterLICM
                    _            -> error "Unknown regalloc option"
      mir <- o .: if when' == BeforeLICM then ("MIR" :: Text) else ("LICM" :: Text)
      return $ OptGraph gid when' mir

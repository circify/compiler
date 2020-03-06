{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AST.Regalloc where
import           AST.LIR
import           Control.Monad.State.Strict (liftIO, unless)
import           Data.Aeson
import qualified Data.Map                   as M
import           Data.Text                  hiding (foldl, unwords)
import           Data.Word
import           GHC.Generics

data RegallocMap = RegallocMap { beforeRegalloc :: AllocationMap
                               , afterRegalloc  ::AllocationMap
                               }
                 deriving (Show)

emptyRegallocMap :: RegallocMap
emptyRegallocMap = RegallocMap M.empty M.empty

type AllocationMap = M.Map Word32 LIR

makeRegallocMap :: [Graph] -> RegallocMap
makeRegallocMap graphs =
  foldl (\(RegallocMap before after) (Graph idx when lir) ->
           RegallocMap (if when == BeforeRegalloc then M.insert idx lir before else before)
                       (if when == AfterRegalloc then M.insert idx lir after else after)
        ) emptyRegallocMap graphs

data Graph = Graph Word32 When LIR
           deriving (Show)
data When = BeforeRegalloc | AfterRegalloc
          deriving (Eq, Ord, Show)

instance FromJSON Graph where
    parseJSON = withObject "comparison" $ \o -> do
      gid <- o .: ("graphId" :: Text)
      (when :: Text) <- o .: ("name" :: Text)
      let when' = case when of
                    "beforeRegisterAllocation" -> BeforeRegalloc
                    "afterRegisterAllocation"  -> AfterRegalloc
                    _                          -> error "Unknown regalloc option"
      lir <- o .: ("lir" :: Text)
      return $ Graph gid when' lir


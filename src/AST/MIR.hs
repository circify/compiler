{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module AST.MIR ( module AST.MIR
               , module AST.Typed
               ) where
import           AST.Typed
import           Control.Monad.State.Strict (join, unless, when)
import           Data.Aeson                 hiding (Object)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import           Data.Text                  hiding (concatMap, foldl, map,
                                             unwords)
import           Data.Word
import           GHC.Generics
import           Prelude                    hiding (id)

{-|

This module is an AST for IonMonkey's MIR.

-}

type MBlockId = Word32
type MNodeId = Word32

-- ^ Raw Firefox MIR
data MIR = MIR { blocks :: [MBlock] }
         deriving (Show, Generic)

instance FromJSON MIR where

data MBlock = MBlock { blockId     :: MBlockId
                     , blockKind   :: Text
                     , unreachable :: Bool
                     , marked      :: Bool
                     , preds       :: [MBlockId]
                     , succs       :: [MBlockId]
                     , resumePoint :: ResumePoint
                     , phiNodes    :: [MNode]
                     , instrs      :: [MNode]
                     }
            deriving (Show, Generic)

instance FromJSON MBlock where
    -- parseJSON = withObject "lblock" $ \o -> do
    --   blockId <- o .: ("id" :: Text)
    --   entries <- o .: ("entryMoves" :: Text)
    --   exits <- o .: ("exitMoves" :: Text)
    --   nodes <- o .: ("nodes" :: Text)
    --   return $ LBlock blockId entries exits nodes

data ResumePoint = ResumePoint { resumeKind :: Text
                               , resumeMode :: Text
                               , resumeOps  :: [TypedOp]
                               }
                 deriving (Show, Generic)

instance FromJSON ResumePoint where

data TypedOp = TypedOp { opName :: Text
                       , opType :: Text
                       }
             deriving (Show, Generic)

instance FromJSON TypedOp where

data MNode = MNode { kind            :: Text
                   , id              :: MNodeId
                   , ty              :: Text
                   , opName          :: Text
                   , operands        :: [TypedOp]
                   , nodeResumePoint :: Maybe ResumePoint
                   }
           deriving (Generic, Show)

instance FromJSON MNode where
    -- parseJSON = withObject "node" $ \o -> do
    --   id <- o .: ("id" :: Text)
    --   op <- o .: ("operation" :: Text)
    --   iscall <- o .: ("isCall" :: Text)
    --   reci <- o .: ("recoversInput" :: Text)
    --   cpr <- o .: ("callPreservesRegs" :: Text)
    --   ops <- o .: ("operands" :: Text)
    --   defs <- o .: ("defs" :: Text)
    --   temps <- o .: ("temps" :: Text)
    --   succs <- o .: ("successors" :: Text)
    --   im <- o .: ("inputMoves" :: Text)
    --   frm <- o .: ("fixReuseMoves" :: Text)
    --   ma <- o .: ("movesAfter" :: Text)
    --   return $ LNode id op iscall reci cpr ops defs temps succs im frm ma

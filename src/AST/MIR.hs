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
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes, fromJust, isJust)
import           Data.Text                  hiding (concatMap, foldl,
                                             intercalate, map, unwords)
import           Data.Word
import           Debug.Trace
import           GHC.Generics
import           Prelude                    hiding (id)

{-|

This module is an AST for IonMonkey's MIR.

We do all the shit explicitly because debugging and i dont know what types things
will end up being, i know its gross but it doesnt matter

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
                     , resumePoint :: BlockResumePoint
                     , phiNodes    :: [MNode]
                     , instrs      :: [MNode]
                     }
            deriving (Show, Generic, Eq)

makeBlockMap :: [MBlock] -> M.Map MBlockId MBlock
makeBlockMap = foldl (\m b -> M.insert (blockId b) b m) M.empty

makeNodeMap :: [MNode] -> M.Map MNodeId MNode
makeNodeMap = foldl insertNode M.empty
  where insertNode m n = M.insert (id n) n m

instance FromJSON MBlock where
    parseJSON = withObject "mblock" $ \o -> do
      blockId     <- o .: ("id" :: Text)
      blockKind   <- o .: ("kind" :: Text)
      unreachable <- o .: ("unreachable" :: Text)
      marked      <- o .: ("marked" :: Text)
      preds       <- o .: ("predecessors" :: Text)
      succs       <- o .: ("successors" :: Text)
      resume      <- o .: ("resumePoint" :: Text)
      phis        <- o .: ("phiNodes" :: Text)
      instrs      <- o .: ("instructionNodes" :: Text)
      return $ MBlock blockId blockKind unreachable marked preds succs resume phis instrs

data BlockResumePoint = BlockResumePoint { bresumeKind :: Text
                                         , bresumeMode :: Text
                                         , bresumeOps  :: [TypedOp]
                                         }
                      deriving (Show, Generic, Eq)

instance FromJSON BlockResumePoint where
    parseJSON = withObject "brp" $ \o -> do
      rk <- o .: ("kind" :: Text)
      rm <- o .: ("mode" :: Text)
      ro <- o .: ("operands" :: Text)
      return $ BlockResumePoint rk rm ro

data TypedOp = TypedOp { topName    :: Text
                       , topType    :: Text
                       , topId      :: Int
                       , topBlockId :: Int
                       }
             deriving (Show, Generic, Eq)

prettyOp :: TypedOp -> String
prettyOp to = unwords ["{", show $ topId to, ",", show $ topBlockId to, "}"]

instance FromJSON TypedOp where
    parseJSON = withObject "to" $ \o -> do
      n <- o .: ("name" :: Text)
      t <- o .: ("type" :: Text)
      i <- o .: ("id" :: Text)
      b <- o .: ("blockId" :: Text)
      return $ TypedOp n t i b

data MNode = MNode { kind            :: Text
                   , id              :: MNodeId
                   , ty              :: Text
                   , opName          :: Text
                   , operands        :: [TypedOp]
                   , nodeResumePoint :: Maybe ResumePoint
                   }
           deriving (Generic, Show, Eq)

prettyNode :: MNode -> String
prettyNode node = unwords [ "id"
                          , show $ id node
                          , "="
                          , show $ opName node
                          , "("
                          , intercalate " " $ map prettyOp $ operands node
                          , ")"
                          ]

instance FromJSON MNode where
    parseJSON = withObject "n" $ \o -> do
      k  <- o .:  ("kind" :: Text)
      i  <- o .:  ("id" :: Text)
      t  <- o .:  ("type" :: Text)
      on <- o .:  ("opName" :: Text)
      os <- o .:  ("operands" :: Text)
      r  <- o .:? ("resumePoint" :: Text)
      return $ MNode k i t on os r

data ResumePoint = ResumePoint { resumeMode :: Text
                               , resumeAt   :: Maybe Int
                               , resumeOps  :: [TypedOp]
                               }
                 deriving (Show, Generic, Eq)


instance FromJSON ResumePoint where
    parseJSON = withObject "rp" $ \o -> do
      rm <- o .:  ("mode" :: Text)
      ra <- o .:? ("resumeAt" :: Text)
      ro <- o .:  ("operands" :: Text)
      return $ ResumePoint rm ra ro

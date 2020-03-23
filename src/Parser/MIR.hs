{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.LIR where
import           AST.MIR
import           Control.Monad (foldM, forM_, unless, when)
import           Data.Aeson
import qualified Data.Map      as M
import           Data.Maybe    (fromJust, isJust, isNothing)
import           Data.Text     hiding (length, null, unwords, zip)
import           Prelude       hiding (id)

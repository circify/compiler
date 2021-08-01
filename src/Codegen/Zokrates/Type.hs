module Codegen.Zokrates.Type
  ( Type(..)
  , naryReturnType
  , naryReturnTypeName
  ) where

import qualified Data.Map.Strict               as Map

data Type = Uint Int
          | Bool
          | Field
          | Struct String (Map.Map String Type)
          | Array Int Type
          deriving (Show,Eq)

naryReturnTypeName = "retty"

naryReturnType :: [Type] -> Type
naryReturnType ts =
  Struct naryReturnTypeName $ Map.fromList $ zip (show <$> [(0 :: Int) ..]) ts

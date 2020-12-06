module ConsProp.Apron.Texpr1 where
import           ConsProp.Apron.AbstractMonad
import           ConsProp.Apron.TexprOp
import           Control.Monad.State.Strict (liftIO)

type Value = Int

data Texpr1 = Cst Value
            | Var VarName
            | UnOp OpType Texpr1
            | BinOp OpType Texpr1 Texpr1
  deriving (Eq, Show)

-- Constructors, etc

texprMakeConstant :: Value -> Abstract Texpr1
texprMakeConstant v = do
  return (Cst v)

texprMakeLeafVar :: VarName -> Abstract Texpr1
texprMakeLeafVar v = do
  return (Var v)

-- Ignore Rounding
texprMakeUnOp :: OpType
              -> Texpr1
              -> RoundingType
              -> RoundingDir
              -> Abstract Texpr1
texprMakeUnOp op t _ _= do
  return (UnOp op t)

texprMakeBinOp :: OpType
               -> Texpr1
               -> Texpr1
               -> RoundingType
               -> RoundingDir
               -> Abstract Texpr1
texprMakeBinOp op t1 t2 _ _ = do
  return (BinOp op t1 t2)
